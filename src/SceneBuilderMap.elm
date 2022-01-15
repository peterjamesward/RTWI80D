module SceneBuilderMap exposing (..)

import Angle exposing (Angle)
import Angle.Interval as Interval
import BoundingBox2d
import Direction2d
import DomainModel exposing (..)
import Json.Encode as E
import Length exposing (Meters)
import Quantity
import Quantity.Interval as Interval
import TrackLoaded exposing (TrackLoaded)


lineToJSON : List GPXSource -> E.Value
lineToJSON points =
    -- JSON suitable for Mapbox API to add polyline for route.
    let
        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            List.map
                (\{ longitude, latitude, altitude } ->
                    DomainModel.lngLatPair ( Direction2d.toAngle longitude, latitude )
                )
                points
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]


pointsToJSON : List GPXSource -> E.Value
pointsToJSON points =
    -- Similar but each point is a feature so it is draggable.
    --var geojson = {
    --    'type': 'FeatureCollection',
    --    'features': [
    --        {
    --            'type': 'Feature',
    --            'geometry': {
    --                'type': 'Point',
    --                'coordinates': [0, 0]
    --            }
    --        }
    --    ]
    --};
    let
        features =
            List.map makeFeature points

        makeFeature tp =
            E.object
                [ ( "type", E.string "Feature" )
                , ( "geometry", point tp )
                ]

        coordinates pt =
            DomainModel.lngLatPair ( Direction2d.toAngle pt.longitude, pt.latitude )

        point tp =
            E.object
                [ ( "type", E.string "Point" )
                , ( "coordinates", coordinates tp )
                ]
    in
    E.object
        [ ( "type", E.string "FeatureCollection" )
        , ( "features", E.list identity features )
        ]


useBounds : Float -> Float -> Float -> Float -> TrackLoaded -> E.Value
useBounds minLon maxLon minLat maxLat track =
    let
        easternEdge =
            Angle.degrees maxLon

        westernEdge =
            Angle.degrees minLon

        visibleInterval =
            Interval.fromEndpoints ( westernEdge, easternEdge )

        mapLocation : GPXSource -> ( Angle, Angle )
        mapLocation point =
            let
                { longitude, latitude, altitude } =
                    point
            in
            ( Direction2d.toAngle longitude, latitude )

        makeVisibleSegment : PeteTree -> E.Value
        makeVisibleSegment node =
            lngLatPair <| mapLocation <| Tuple.second <| sourceData node

        --TODO: Render based on Map bounding box.
        --NOTE: We ignore any dateline concerns here.
        estimatedLeafCounter treeNode ( runningCount, maxDepth ) =
            let
                nodeInterval =
                    Interval.fromEndpoints ( mostWesterly treeNode, mostEasterly treeNode )
            in
            if nodeInterval |> Interval.intersects visibleInterval then
                -- It's at least partly visible
                case treeNode of
                    Leaf leaf ->
                        ( runningCount + 1, maxDepth )

                    Node node ->
                        let
                            ( leftCount, leftDepth ) =
                                estimatedLeafCounter node.left ( runningCount, maxDepth )

                            ( rightCount, rightDepth ) =
                                estimatedLeafCounter node.right ( runningCount, maxDepth )
                        in
                        ( runningCount + leftCount + rightCount
                        , 1 + max leftDepth rightDepth
                        )

            else
                ( runningCount, maxDepth )

        ( visibleCount, visibleDepth ) =
            estimatedLeafCounter track.trackTree ( 0, 0 )

        useDepth =
            clamp 10 22 <|
                round <|
                    30
                        - logBase 2 (toFloat visibleCount)

        _ =
            Debug.log "INTERESTING" ( visibleCount, visibleDepth, useDepth )

        renderTree : Int -> PeteTree -> List E.Value -> List E.Value
        renderTree depth someNode accum =
            let
                nodeInterval =
                    Interval.fromEndpoints ( mostWesterly someNode, mostEasterly someNode )
            in
            if nodeInterval |> Interval.intersects visibleInterval then
                -- It's at least partly visible
                case someNode of
                    Leaf leafNode ->
                        makeVisibleSegment someNode :: accum

                    Node notLeaf ->
                        if depth <= 0 then
                            makeVisibleSegment someNode :: accum

                        else
                            accum
                                |> renderTree (depth - 1) notLeaf.right
                                |> renderTree (depth - 1) notLeaf.left

            else
                []

        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            renderTree useDepth track.trackTree []
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]


renderMapJson : TrackLoaded -> E.Value
renderMapJson track =
    let
        mapLocation : GPXSource -> ( Angle, Angle )
        mapLocation point =
            let
                { longitude, latitude, altitude } =
                    point
            in
            ( Direction2d.toAngle longitude, latitude )

        makeVisibleSegment : PeteTree -> E.Value
        makeVisibleSegment node =
            lngLatPair <| mapLocation <| Tuple.second <| sourceData node

        renderTree : Int -> PeteTree -> List E.Value -> List E.Value
        renderTree depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    makeVisibleSegment someNode :: accum

                Node notLeaf ->
                    if depth <= 0 then
                        makeVisibleSegment someNode :: accum

                    else
                        accum
                            |> renderTree (depth - 1) notLeaf.right
                            |> renderTree (depth - 1) notLeaf.left

        --TODO: Render based on Map bounding box.
        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            renderTree 10 track.trackTree []
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]
