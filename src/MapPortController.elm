port module MapPortController exposing (..)

import Actions exposing (ToolAction(..))
import Angle
import Direction2d
import DomainModel exposing (..)
import Json.Decode as D exposing (Decoder, at, field, string)
import Json.Encode as E
import Length
import MapboxKey exposing (mapboxKey)
import SceneBuilderMap
import TrackLoaded exposing (TrackLoaded)


type MapMsg
    = MapPortMessage E.Value


type alias MapInfo =
    -- Mainly used to set the map up.
    { mapZoom : Float -- track values from user map interactions.
    , centreLon : Float
    , centreLat : Float
    }


defaultMapInfo : MapInfo
defaultMapInfo =
    MapInfo 0.0 0.0 0.0


port mapCommands : E.Value -> Cmd msg


port mapResponses : (E.Value -> msg) -> Sub msg


createMap : MapInfo -> Cmd msg
createMap info =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Init" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float info.centreLon )
            , ( "lat", E.float info.centreLat )
            , ( "zoom", E.float info.mapZoom )
            ]


refreshMap : Cmd msg
refreshMap =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Repaint" )
            , ( "token", E.string mapboxKey )
            ]


centreMapOnCurrent : TrackLoaded -> Cmd msg
centreMapOnCurrent track =
    let
        { longitude, latitude, altitude } =
            gpxPointFromIndex track.currentPosition track.trackTree
    in
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Centre" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
            , ( "lat", E.float <| Angle.inDegrees latitude )
            ]


addTrackToMap : TrackLoaded -> Cmd msg
addTrackToMap track =
    -- This is to add the route as a polyline.
    -- We will separately add track points as draggable features.
    let
        { longitude, latitude, altitude } =
            gpxPointFromIndex track.currentPosition track.trackTree
    in
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Track" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
            , ( "lat", E.float <| Angle.inDegrees latitude )
            , ( "zoom", E.float 10.0 )
            , ( "data", SceneBuilderMap.renderMapJson track ) -- Route as polyline
            , ( "points", E.null ) --trackPointsToJSON track ) -- Make track points draggable
            ]

newTrackRendering : E.Value -> Cmd msg
newTrackRendering data =
    -- Selective rendering requires us to redraw.
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Render" )
            , ( "token", E.string mapboxKey )
            , ( "data", data ) -- Route as polyline
            , ( "points", E.null ) -- Not used but required
            ]


showPreview : String -> String -> String -> E.Value -> Cmd msg
showPreview tag shape colour geoJson =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "ShowPreview" )
            , ( "token", E.string mapboxKey )
            , ( "label", E.string tag )
            , ( "shape", E.string shape )
            , ( "colour", E.string colour )
            , ( "data", geoJson )
            ]


hidePreview : String -> Cmd msg
hidePreview tag =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "HidePreview" )
            , ( "token", E.string mapboxKey )
            , ( "label", E.string tag )
            ]



--addMarkersToMap :
--    Track
--    -> List E.Value
--    -> Cmd msg
--addMarkersToMap track previews =
--    let
--        realWorldPosition tp =
--            Track.withoutGhanianTransform track tp.xyz
--
--        encodePos ( lon, lat, ele ) =
--            E.object
--                [ ( "lon", E.float lon )
--                , ( "lat", E.float lat )
--                ]
--    in
--    commandPort <|
--        E.object
--            [ ( "Cmd", E.string "Mark" )
--            , ( "orange", encodePos <| realWorldPosition track.currentNode )
--            , case track.markedNode of
--                Just mark ->
--                    ( "purple", encodePos <| realWorldPosition mark )
--
--                Nothing ->
--                    ( "ignore", E.null )
--            , ( "previews", E.list identity previews )
--            ]


msgDecoder : Decoder String
msgDecoder =
    field "msg" string


update :
    MapMsg
    -> TrackLoaded
    -> List (ToolAction msg)
update mapMsg track =
    case mapMsg of
        MapPortMessage value ->
            processMapPortMessage track value


processMapPortMessage :
    TrackLoaded
    -> E.Value
    -> List (ToolAction msg)
processMapPortMessage track json =
    -- Only interested in click and bounding box information.
    let
        jsonMsg =
            D.decodeValue msgDecoder json
    in
    case jsonMsg of
        Ok "click" ->
            --{ 'msg' : 'click'
            --, 'lat' : e.lat()
            --, 'lon' : e.lon()
            --} );
            case
                ( D.decodeValue (D.field "lat" D.float) json
                , D.decodeValue (D.field "lon" D.float) json
                )
            of
                ( Ok lat1, Ok lon1 ) ->
                    let
                        gpxPoint =
                            { longitude = Direction2d.fromAngle <| Angle.degrees lon1
                            , latitude = Angle.degrees lat1
                            , altitude = Length.meters 0.0
                            }

                        index = 0
                            --DomainModel.nearestToLonLat gpxPoint track.trackTree
                    in
                    [ SetCurrent index ]

                _ ->
                    []

        Ok "bounds" ->
            let
                minLon =
                    D.decodeValue (at [ "sw", "lng" ] D.float) json

                maxLon =
                    D.decodeValue (at [ "ne", "lng" ] D.float) json

                minLat =
                    D.decodeValue (at [ "sw", "lat" ] D.float) json

                maxLat =
                    D.decodeValue (at [ "ne", "lat" ] D.float) json
            in
            case ( ( minLon, maxLon ), ( minLat, maxLat ) ) of
                ( ( Ok aMinLon, Ok aMaxLon ), ( Ok aMinLat, Ok aMaxLat ) ) ->
                    [ SetBounds aMinLon aMaxLon aMinLat aMaxLat ]

                _ ->
                    []

        _ ->
            []
