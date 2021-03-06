module DomainModel exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction2d exposing (Direction2d)
import Json.Encode as E
import Length exposing (Length, Meters, inMeters)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Sphere3d exposing (Sphere3d)
import Spherical as Spherical exposing (range)


type alias GPXSource =
    -- Being a raw line of data from GPX file.
    { longitude : Direction2d LocalCoords
    , latitude : Angle
    , altitude : Length.Length
    }


type alias EarthPoint =
    Point3d Length.Meters LocalCoords


type alias RoadSection =
    -- Can be based between two 'fundamental' points from GPX, or an assembly of them.
    -- Bounding box and Sphere needed for culling in nearness tests.
    -- Keeping track of longitude tricky because of IDL.
    { sourceData : ( GPXSource, GPXSource )

    -- For rapid location of points using non-map views...
    , trueLength : Length.Length
    , skipCount : Int

    -- For efficient rendering, knowing we do not cross the date line!
    , minLongitude : Angle
    , maxLongitude : Angle
    }


type
    PeteTree
    -- Absurdly simple tree may work (does, rather spiffingly).
    = Leaf RoadSection
    | Node
        { nodeContent : RoadSection
        , left : PeteTree
        , right : PeteTree
        }


asRecord : PeteTree -> RoadSection
asRecord treeNode =
    -- Because is daft writing accessors for every field.
    case treeNode of
        Leaf section ->
            section

        Node node ->
            node.nodeContent


sourceData : PeteTree -> ( GPXSource, GPXSource )
sourceData treeNode =
    treeNode |> asRecord |> .sourceData


trueLength : PeteTree -> Length
trueLength treeNode =
    treeNode |> asRecord |> .trueLength


skipCount : PeteTree -> Int
skipCount treeNode =
    case treeNode of
        Leaf leaf ->
            1

        Node node ->
            node.nodeContent.skipCount


maxLongitude treeNode =
    treeNode |> asRecord |> .maxLongitude


minLongitude treeNode =
    treeNode |> asRecord |> .minLongitude


makeRoadSection : GPXSource -> GPXSource -> GPXSource -> RoadSection
makeRoadSection reference earth1 earth2 =
    let
        range : Length.Length
        range =
            Length.meters <|
                Spherical.range
                    ( Direction2d.toAngle earth1.longitude, earth1.latitude )
                    ( Direction2d.toAngle earth2.longitude, earth2.latitude )
    in
    { sourceData = ( earth1, earth2 )
    , trueLength = range
    , skipCount = 1
    , minLongitude =
        Quantity.min
            (Direction2d.toAngle <| earth1.longitude)
            (Direction2d.toAngle <| earth2.longitude)
    , maxLongitude =
        Quantity.max
            (Direction2d.toAngle <| earth1.longitude)
            (Direction2d.toAngle <| earth2.longitude)
    }


treeFromList : List GPXSource -> Maybe PeteTree
treeFromList track =
    -- Build the skeletal tree of nodes, then attach the leaves from the input list.
    -- Should be much quicker than recursively splitting the list, for large lists.
    -- First point is arbitrary reference for conformal projection (TBC).
    let
        referencePoint =
            -- From which, arbitrarily, we compute metre offsets.
            -- We won't be here without a track, so default is harmless.
            List.head track
                |> Maybe.withDefault (GPXSource Direction2d.x Quantity.zero Quantity.zero)

        numberOfSegments =
            List.length track - 1

        combineInfo : PeteTree -> PeteTree -> RoadSection
        combineInfo info1 info2 =
            { sourceData = ( Tuple.first (sourceData info1), Tuple.second (sourceData info2) )
            , trueLength = Quantity.plus (trueLength info1) (trueLength info2)
            , skipCount = skipCount info1 + skipCount info2
            , minLongitude =
                Quantity.min
                    (minLongitude info1)
                    (minLongitude info2)
            , maxLongitude =
                Quantity.max
                    (maxLongitude info1)
                    (maxLongitude info2)
            }

        treeBuilder : Int -> List GPXSource -> ( Maybe PeteTree, List GPXSource )
        treeBuilder n pointStream =
            case ( n < 2, pointStream ) of
                ( True, v1 :: v2 :: vvvv ) ->
                    -- Take two vectors for this Leaf, but only consume one.
                    ( Just <| Leaf <| makeRoadSection referencePoint v1 v2, v2 :: vvvv )

                ( True, anythingElse ) ->
                    -- Hmm. This shouldn't have happened if we've done our numbers right.
                    ( Nothing, anythingElse )

                ( False, vvvv ) ->
                    -- Make a non-leaf Node, recursively
                    let
                        leftSize =
                            n // 2

                        rightSize =
                            n - leftSize

                        ( left, remainingAfterLeft ) =
                            treeBuilder leftSize vvvv

                        ( right, remainingAfterRight ) =
                            treeBuilder rightSize remainingAfterLeft
                    in
                    case ( left, right ) of
                        -- Should have returned _something_ but we're forced to check
                        ( Just leftSubtree, Just rightSubtree ) ->
                            ( Just <|
                                Node
                                    { nodeContent = combineInfo leftSubtree rightSubtree
                                    , left = leftSubtree
                                    , right = rightSubtree
                                    }
                            , remainingAfterRight
                            )

                        _ ->
                            ( Nothing, remainingAfterRight )
    in
    treeBuilder numberOfSegments track |> Tuple.first


leafFromIndex : Int -> PeteTree -> PeteTree
leafFromIndex index treeNode =
    case treeNode of
        Leaf info ->
            treeNode

        Node info ->
            if index < skipCount info.left then
                leafFromIndex index info.left

            else
                leafFromIndex (index - skipCount info.left) info.right


gpxPointFromIndex : Int -> PeteTree -> GPXSource
gpxPointFromIndex index treeNode =
    case treeNode of
        Leaf info ->
            if index <= 0 then
                Tuple.first info.sourceData

            else
                Tuple.second info.sourceData

        Node info ->
            if index < skipCount info.left then
                gpxPointFromIndex index info.left

            else
                gpxPointFromIndex (index - skipCount info.left) info.right


gpxDistance : GPXSource -> GPXSource -> Length.Length
gpxDistance p1 p2 =
    let
        lon p =
            p.longitude |> Direction2d.toAngle
    in
    Length.meters <|
        range
            ( Direction2d.toAngle p1.longitude, p1.latitude )
            ( Direction2d.toAngle p2.longitude, p2.latitude )



--
--nearestToLonLat :
--    GPXSource
--    -> PeteTree
--    -> Int
--nearestToLonLat click treeNode =
--    -- Only for click detect on Map view.
--    let
--        helper withNode skip =
--            case withNode of
--                Leaf leaf ->
--                    -- Use whichever point is closest.
--                    let
--                        startDistance =
--                            gpxDistance click <| Tuple.first leaf.sourceData
--
--                        endDistance =
--                            gpxDistance click <| Tuple.second leaf.sourceData
--                    in
--                    if startDistance |> Quantity.lessThanOrEqualTo endDistance then
--                        ( skip, startDistance )
--
--                    else
--                        ( skip + 1, endDistance )
--
--                Node node ->
--                    -- The trick here is effective culling, but better to search
--                    -- unnecessarily than to miss the right point.
--                    let
--                        ( inLeftSpan, inRightSpan ) =
--                            ( isLongitudeContained click.longitude node.left
--                            , isLongitudeContained click.longitude node.right
--                            )
--
--                        --_ =
--                        --    Debug.log "SPANS" ( inLeftSpan, inRightSpan )
--                    in
--                    case ( inLeftSpan, inRightSpan ) of
--                        ( True, True ) ->
--                            -- Could go either way, best check both.
--                            let
--                                ( leftBestIndex, leftBestDistance ) =
--                                    helper node.left skip
--
--                                ( rightBestIndex, rightBestDistance ) =
--                                    helper node.right (skip + skipCount node.left)
--                            in
--                            if leftBestDistance |> Quantity.lessThanOrEqualTo rightBestDistance then
--                                ( leftBestIndex, leftBestDistance )
--
--                            else
--                                ( rightBestIndex, rightBestDistance )
--
--                        ( True, False ) ->
--                            helper node.left skip
--
--                        ( False, True ) ->
--                            helper node.right (skip + skipCount node.left)
--
--                        ( False, False ) ->
--                            let
--                                ( leftDistance, rightDistance ) =
--                                    ( rotationAwayFrom click.longitude node.left
--                                    , rotationAwayFrom click.longitude node.right
--                                    )
--                            in
--                            if leftDistance |> Quantity.lessThanOrEqualTo rightDistance then
--                                helper node.left skip
--
--                            else
--                                helper node.right (skip + skipCount node.left)
--    in
--    Tuple.first <| helper treeNode 0


lngLatPair : ( Angle, Angle ) -> E.Value
lngLatPair ( longitude, latitude ) =
    E.list E.float [ Angle.inDegrees longitude, Angle.inDegrees latitude ]
