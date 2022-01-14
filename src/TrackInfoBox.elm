module TrackInfoBox exposing (..)

import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import FlatColors.ChinesePalette
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showDecimal2, showLongMeasure, showShortMeasure)


trackInfoList : List ( Element msg, PeteTree -> Element msg )
trackInfoList =
    [ ( text "Points", asRecord >> .skipCount >> (+) 1 >> String.fromInt >> text )
    , ( text "Length", asRecord >> .trueLength >> showLongMeasure False >> text )
    ]


trackInfoBox : Maybe TrackLoaded -> Element msg
trackInfoBox maybeTrack =
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        case maybeTrack of
            Just track ->
                row
                    [ padding 10
                    , spacing 5
                    ]
                    [ column [ spacing 5 ] <| List.map (\( txt, _ ) -> txt) trackInfoList
                    , column [ spacing 5 ] <| List.map (\( _, fn ) -> fn track.trackTree) trackInfoList
                    ]

            Nothing ->
                row
                    [ padding 10
                    , spacing 5
                    ]
                    [ column [ spacing 5 ] <| List.map (\( txt, _ ) -> txt) trackInfoList
                    , column [ spacing 5 ] <| List.map (always <| text "- no data -") trackInfoList
                    ]
