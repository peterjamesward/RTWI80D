module ToolsController exposing (..)

import Actions exposing (ToolAction)
import Element exposing (..)
import Element.Background as Background exposing (color)
import Element.Border as Border exposing (roundEach)
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.SwedishPalette
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import List.Extra
import TrackInfoBox
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (contrastingColour, neatToolsBorder, useIcon)


type ToolState
    = Expanded
    | Contracted
    | Disabled


type ToolDock
    = DockUpperLeft
    | DockLowerLeft
    | DockUpperRight
    | DockLowerRight
    | DockBottom
    | DockNone


type ToolType
    = ToolTrackInfo


type alias Options =
    -- Tool specific options
    { tools : List ToolEntry
    }


defaultOptions : Options
defaultOptions =
    { tools = defaultTools
    }


type ToolMsg
    = ToolStateToggle ToolType ToolState
    | ToolNoOp


type alias ToolEntry =
    { toolType : ToolType
    , label : String
    , info : String
    , video : Maybe String
    , state : ToolState
    , dock : ToolDock
    , tabColour : Element.Color
    , textColour : Element.Color
    , isPopupOpen : Bool
    }


defaultTools : List ToolEntry
defaultTools =
    -- One list or five, or six? Try one. Arguably a Dict but POITROAE.
    [ trackInfoBox
    ]


trackInfoBox : ToolEntry
trackInfoBox =
    { toolType = ToolTrackInfo
    , label = "Summary info"
    , info = "Here is some useful information"
    , video = Nothing
    , state = Expanded
    , dock = DockUpperLeft
    , tabColour = FlatColors.AussiePalette.beekeeper
    , textColour = contrastingColour FlatColors.AussiePalette.beekeeper
    , isPopupOpen = False
    }


nextToolState : ToolState -> ToolState
nextToolState state =
    case state of
        Expanded ->
            Contracted

        Contracted ->
            Expanded

        Disabled ->
            Disabled


refreshOpenTools :
    Maybe TrackLoaded
    -> Options
    -> ( Options, List (ToolAction msg) )
refreshOpenTools isTrack options =
    -- Track, or something has changed; tool data is stale.
    -- Same impact as tools being opened, so we'll re-use that.
    let
        refreshOpenTool entry ( updatedModel, actions ) =
            if entry.state == Expanded then
                let
                    ( incrementalModel, incrementalActions ) =
                        toolStateHasChanged entry.toolType Expanded isTrack updatedModel
                in
                ( incrementalModel, incrementalActions ++ actions )

            else
                ( updatedModel, actions )
    in
    options.tools |> List.foldl refreshOpenTool ( options, [] )


toolStateHasChanged :
    ToolType
    -> ToolState
    -> Maybe TrackLoaded
    -> Options
    -> ( Options, List (ToolAction msg) )
toolStateHasChanged toolType newState isTrack options =
    case toolType of
        ToolTrackInfo ->
            ( options, [] )


update :
    ToolMsg
    -> Maybe TrackLoaded
    -> (ToolMsg -> msg)
    -> Options
    -> ( Options, List (ToolAction msg) )
update toolMsg isTrack msgWrapper options =
    ( options, [] )



--View stuff


viewTool :
    (ToolMsg -> msg)
    -> Maybe TrackLoaded
    -> Options
    -> ToolEntry
    -> Element msg
viewTool msgWrapper isTrack options toolEntry =
    column
        [ width fill
        , spacing 0
        , Border.width 2
        , Border.color toolEntry.tabColour
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , spacing 8
            , padding 4
            , Background.color toolEntry.tabColour
            , Font.color toolEntry.textColour
            ]
            [ el [ centerX ]
                <| text toolEntry.label


            ]
        , if toolEntry.state == Expanded then
            viewToolByType msgWrapper toolEntry isTrack options

          else
            none
        ]

viewToolByType :
    (ToolMsg -> msg)
    -> ToolEntry
    -> Maybe TrackLoaded
    -> Options
    -> Element msg
viewToolByType msgWrapper entry isTrack options =
    case entry.toolType of
        ToolTrackInfo ->
            TrackInfoBox.trackInfoBox isTrack
