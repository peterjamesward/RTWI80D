module ViewPureStyles exposing (..)

import Color exposing (blue)
import ColourPalette exposing (buttonBackground, buttonShadow, buttonText, collapsedTabBorder, radioButtonDefault, radioButtonSelected, radioButtonText, scrollbarBackground)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Thumb, thumb)
import FeatherIcons
import FlatColors.BritishPalette
import FlatColors.ChinesePalette
import Html.Attributes exposing (style)


defaultRowLayout =
    []


toolRowLayout =
    [ spacing 10
    , paddingXY 20 10
    , width fill
    ]


edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


defaultColumnLayout =
    [ spacing 5, padding 5, alignTop, width fill ]


commonLayoutStyles =
    [ padding 2
    , spacing 5
    , width fill
    , height fill
    , Font.size 14
    , Font.family
        [ Font.typeface "Open Sans"
        , Font.sansSerif
        ]
    ]


prettyButtonStyles =
    [ padding 10
    , Border.width 2
    , Border.rounded 4
    , Border.color FlatColors.ChinesePalette.bayWharf
    , Background.color FlatColors.ChinesePalette.frenchSkyBlue
    , Font.color FlatColors.ChinesePalette.prestigeBlue
    , Font.size 16

    --, mouseOver
    --    [ Background.color buttonText, Font.color buttonBackground ]
    --, focused
    --    [ Border.shadow { offset = ( 4, 0 ), size = 3, blur = 5, color = buttonShadow } ]
    ]


disabledButtonStyles =
    [ padding 10
    , Border.width 2
    , Border.rounded 4
    , Border.color FlatColors.BritishPalette.riseNShine
    , Background.color FlatColors.BritishPalette.riseNShine
    , Font.color FlatColors.ChinesePalette.antiFlashWhite
    , Font.size 16
    , width fill
    ]


conditionallyVisible : Bool -> Element msg -> Element msg
conditionallyVisible test element =
    -- This turns out to be the secret sauce for easier map integration.
    -- It means we can pre-load a Mapbox map element.
    if test then
        el [] element

    else
        el [ htmlAttribute (style "display" "none") ] element


commonShortHorizontalSliderStyles =
    [ height <| px 20
    , width <| px 150
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 150
            , height <| px 2
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


commonShortVerticalSliderStyles =
    [ height <| px 150
    , width <| px 20
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 2
            , height <| px 150
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


checkboxIcon : Bool -> Element msg
checkboxIcon isChecked =
    el
        [ width <| px 32
        , height <| px 32
        , centerY
        , padding 4
        , Border.rounded 4
        , Border.width 2
        , Border.color FlatColors.ChinesePalette.frenchSkyBlue
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 4
            , Background.color <|
                if isChecked then
                    FlatColors.ChinesePalette.bruschettaTomato

                else
                    FlatColors.ChinesePalette.twinkleBlue
            ]
        <|
            none


contrastingColour col =
    let
        { red, green, blue, alpha } =
            toRgb col
    in
    fromRgb
        { red = toFloat <| round <| 1.0 - red
        , green = toFloat <| round <| 1.0 - green
        , blue = toFloat <| round <| 1.0 - blue
        , alpha = 1.0
        }


radioButton label state =
    el
        [ spacing 1
        , padding 6
        , Border.color FlatColors.ChinesePalette.clearChill
        , Border.widthEach <|
            if state == Input.Selected then
                { left = 2, right = 2, top = 2, bottom = 0 }

            else
                { left = 1, right = 1, top = 1, bottom = 0 }

        --, Border.roundEach { topLeft = 6, bottomLeft = 0, topRight = 6, bottomRight = 0 }
        , Background.color <|
            if state == Input.Selected then
                FlatColors.ChinesePalette.antiFlashWhite

            else
                FlatColors.ChinesePalette.peace
        , Font.color <|
            if state == Input.Selected then
                FlatColors.ChinesePalette.prestigeBlue

            else
                FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        el [ centerX, centerY ] <|
            text label


displayName n =
    case n of
        Just s ->
            el [ Font.size 20 ]
                (text s)

        _ ->
            none


wideSliderStyles =
    [ height <| px 24
    , width <| px 400
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 400
            , height <| px 2
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


neatToolsBorder =
    [ Background.color FlatColors.ChinesePalette.antiFlashWhite
    , Border.color FlatColors.ChinesePalette.bruschettaTomato
    , Border.rounded 4
    , Border.width 2
    ]


shortSliderStyles =
    [ height <| px 24
    , width <| px 150
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 150
            , height <| px 2
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


sliderThumb : Thumb
sliderThumb =
    thumb
        [ Element.width (Element.px 16)
        , Element.height (Element.px 16)
        , Border.rounded 4
        , Border.width 1
        , Border.color (Element.rgb 0.5 0.5 0.5)
        , Background.color (Element.rgb 1 1 1)
        ]


useIcon =
    html << FeatherIcons.toHtml [] << FeatherIcons.withSize 20
