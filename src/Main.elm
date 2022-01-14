module Main exposing (main)

import Actions exposing (ToolAction(..))
import Browser exposing (document)
import Browser.Dom as Dom exposing (getViewport, getViewportOf)
import Browser.Events
import Direction2d
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Select as Select
import FlatColors.ChinesePalette
import GeoCodeDecoders exposing (IpInfo)
import GpxParser exposing (parseGPXPoints)
import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
import Http
import Json.Encode as E exposing (string)
import LocalCoords exposing (LocalCoords)
import MapPortController exposing (defaultMapInfo)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Task
import Time
import ToolsController exposing (ToolDock(..), ToolEntry, toolsForDock, trackInfoBox, viewTool)
import TrackLoaded exposing (TrackLoaded)
import ViewContext exposing (ViewContext(..), ViewMode(..))
import ViewMap exposing (MapContext)
import ViewPureStyles exposing (commonLayoutStyles, neatToolsBorder, sliderThumb)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | AdjustTimeZone Time.Zone
    | SetRenderDepth Int
    | SetCurrentPosition Int
    | SetViewMode ViewMode
    | MapPortsMessage MapPortController.MapMsg
    | Resize Int Int
    | GotWindowSize (Result Dom.Error Dom.Viewport)
    | ToolsMsg ToolsController.ToolMsg


type alias Model =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone

    -- Track stuff
    , track : Maybe TrackLoaded

    -- Visuals
    , scene : List (Entity LocalCoords)
    , viewMode : ViewMode
    , viewMapContext : Maybe MapContext

    -- Layout stuff
    , windowSize : ( Float, Float )
    , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )
    , modalMessage : Maybe String

    -- Tools
    , toolOptions : ToolsController.Options
    }


main : Program (Maybe (List Int)) Model Msg
main =
    -- This is the 'main' from OAuth example.
    document
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    -- We stitch in the OAuth init stuff somehow here.
    ( { filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , track = Nothing
      , scene = []
      , viewMode = ViewMap
      , viewMapContext = Nothing
      , windowSize = ( 1000, 800 )
      , contentArea = ( Pixels.pixels 800, Pixels.pixels 500 )
      , modalMessage = Nothing
      , toolOptions = ToolsController.defaultOptions
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.attempt GotWindowSize Dom.getViewport
        , MapPortController.createMap defaultMapInfo
        ]
    )


render : Model -> Model
render model =
    -- This is or should be the one place where rendering for 3D (and similar) happens.
    -- Map is different: it's imperative by nature, and we don't need to retain the json.
    model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        MapPortsMessage mapMsg ->
            case model.track of
                Just track ->
                    let
                        actions =
                            MapPortController.update mapMsg track

                        newModel =
                            performActionsOnModel actions model
                    in
                    ( newModel, performActionCommands actions model )

                Nothing ->
                    ( model, Cmd.none )

        GpxRequested ->
            ( { model | modalMessage = Just "Select GPX file" }
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            ( { model
                | filename = Just (File.name file)
                , modalMessage = Just <| ("Loading " ++ File.name file)
              }
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxLoaded content ->
            let
                gpxTrack =
                    parseGPXPoints content

                trackTree =
                    treeFromList gpxTrack
            in
            case trackTree of
                Just aTree ->
                    let
                        newTrack =
                            { trackTree = aTree
                            , currentPosition = 0
                            , renderDepth = 10
                            , referenceLonLat =
                                List.head gpxTrack
                                    |> Maybe.withDefault (GPXSource Direction2d.x Quantity.zero Quantity.zero)
                            }

                        modelWithTrack =
                            { model
                                | track = Just newTrack
                                , viewMapContext = Just ViewMap.initialiseContext
                                , viewMode = ViewMap
                                , modalMessage = Nothing
                            }

                        ( newOptions, actions ) =
                            ToolsController.refreshOpenTools
                                modelWithTrack.track
                                modelWithTrack.toolOptions

                        modelWithUpdatedTools =
                            { modelWithTrack | toolOptions = newOptions }

                        modelAfterActions =
                            -- e.g. collect previews and render ...
                            performActionsOnModel actions modelWithUpdatedTools
                    in
                    ( modelAfterActions
                    , Cmd.batch
                        [ performActionCommands actions modelAfterActions
                        , showTrackOnMapCentered newTrack
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        SetRenderDepth depth ->
            case model.track of
                Just track ->
                    let
                        newTrack =
                            { track | renderDepth = depth }

                        newModel =
                            { model | track = Just track }
                    in
                    ( newModel, showTrackOnMapCentered newTrack )

                Nothing ->
                    ( model, Cmd.none )

        SetCurrentPosition pos ->
            -- Slider moves pointer and re-centres view.
            -- The actions will re-render and repaint the map.
            -- TODO: Refresh all the open tools.
            case model.track of
                Just track ->
                    let
                        newTrack =
                            { track | currentPosition = pos }

                        newModel =
                            render { model | track = Just newTrack }
                    in
                    ( newModel
                    , performActionCommands [ MapCenterOnCurrent ] newModel
                    )

                Nothing ->
                    ( model, Cmd.none )

        SetViewMode viewMode ->
            case model.track of
                Just track ->
                    let
                        newModel =
                            { model | viewMode = viewMode }
                    in
                    ( newModel, showTrackOnMapCentered track )

                Nothing ->
                    ( model, Cmd.none )

        Resize width height ->
            ( { model | windowSize = ( toFloat width, toFloat height ) }
            , MapPortController.refreshMap
            )

        GotWindowSize result ->
            case result of
                Ok info ->
                    ( { model
                        | windowSize =
                            ( info.viewport.width
                            , info.viewport.height
                            )
                      }
                    , MapPortController.refreshMap
                    )

                Err error ->
                    ( model, Cmd.none )

        ToolsMsg toolMsg ->
            let
                ( newToolOptions, actions ) =
                    -- Some of the actions update the model, some issue commands.
                    ToolsController.update toolMsg model.track ToolsMsg model.toolOptions

                newModel =
                    { model | toolOptions = newToolOptions }

                modelAfterActions =
                    performActionsOnModel actions newModel
            in
            ( modelAfterActions
            , performActionCommands actions modelAfterActions
            )


showModalMessage msg =
    el (centerX :: centerY :: neatToolsBorder) <|
        text msg


view : Model -> Browser.Document Msg
view model =
    { title = "GPXmagic Labs V3 concepts"
    , body =
        [ layout
            (Background.color FlatColors.ChinesePalette.peace
                :: (inFront <|
                        case model.modalMessage of
                            Just msg ->
                                showModalMessage msg

                            Nothing ->
                                none
                   )
                :: commonLayoutStyles
            )
          <|
            column [ width fill ]
                [ topLoadingBar model
                , contentArea model
                ]
        ]
    }


topLoadingBar model =
    let
        loadGpxButton =
            button
                [ padding 5
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                { onPress = Just GpxRequested
                , label = text "Load GPX file"
                }
    in
    row
        (commonLayoutStyles
            ++ [ spacing 20
               , padding 10
               , width fill
               , Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
               , Border.color FlatColors.ChinesePalette.twinkleBlue
               ]
        )
        [ loadGpxButton
        ]


contentArea : Model -> Element Msg
contentArea model =
    let
        ( w, h ) =
            model.contentArea

        slider trackLength =
            Input.slider
                ViewPureStyles.wideSliderStyles
                { onChange = round >> SetCurrentPosition
                , value =
                    case model.track of
                        Just track ->
                            toFloat track.currentPosition

                        Nothing ->
                            0.0
                , label = Input.labelHidden "Current position slider"
                , min = 0
                , max = toFloat <| trackLength - 1
                , step = Just 1
                , thumb = sliderThumb
                }
    in
    -- NOTE that the Map DIV must be constructed once only, or the map gets upset.
    row
        [ centerX, width fill ]
        [ toolsForDock DockUpperLeft ToolsMsg model.track model.toolOptions
        , column
            [ width <| Element.px <| Pixels.inPixels w
            , height <| Element.px <| Pixels.inPixels h
            , alignTop
            , centerX
            ]
            [ ViewMap.view model MapPortsMessage
            , case model.track of
                Just track ->
                    el [ centerX ] <| slider <| 1 + skipCount track.trackTree

                Nothing ->
                    none
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ MapPortController.mapResponses (MapPortsMessage << MapPortController.MapPortMessage)
        , Browser.Events.onResize (\w h -> Resize w h)
        ]


showTrackOnMapCentered : TrackLoaded -> Cmd msg
showTrackOnMapCentered track =
    Cmd.batch
        -- Must repaint track on so that selective rendering works.
        [ MapPortController.addTrackToMap track
        , MapPortController.centreMapOnCurrent track
        ]


performActionsOnModel : List (ToolAction Msg) -> Model -> Model
performActionsOnModel actions model =
    let
        performAction : ToolAction Msg -> Model -> Model
        performAction action mdl =
            case ( action, mdl.track ) of
                ( SetCurrent position, Just track ) ->
                    let
                        newTrack =
                            { track | currentPosition = position }
                    in
                    { mdl | track = Just newTrack }

                _ ->
                    mdl
    in
    List.foldl performAction model actions
        |> render


performActionCommands : List (ToolAction Msg) -> Model -> Cmd Msg
performActionCommands actions model =
    let
        performAction : ToolAction Msg -> Cmd Msg
        performAction action =
            case ( action, model.track ) of
                ( SetCurrent position, Just track ) ->
                    Cmd.batch
                        [ MapPortController.addTrackToMap track

                        --, MapPortController.centreMapOnCurrent track
                        ]

                ( MapCenterOnCurrent, Just track ) ->
                    Cmd.batch
                        [ --MapPortController.addTrackToMap track
                          MapPortController.centreMapOnCurrent track
                        ]

                _ ->
                    Cmd.none
    in
    Cmd.batch <| List.map performAction actions
