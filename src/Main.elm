module Main exposing (main)

import Browser
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import Temperature exposing (Temperature)


type alias Model =
    { unit : TemperatureUnit
    }


type TemperatureUnit
    = Celsius
    | Fahrenheit


type Msg
    = Unit TemperatureUnit


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { unit = Celsius }


view : Model -> Html Msg
view model =
    Html.main_
        []
        [ [ { name = "Cold"
            , winter = Temperature.degreesFahrenheit 20
            , spring = Temperature.degreesFahrenheit 30
            , summer = Temperature.degreesFahrenheit 40
            , fall = Temperature.degreesFahrenheit 30
            , adjust = -1
            }
          , { name = "Temperate"
            , winter = Temperature.degreesFahrenheit 30
            , spring = Temperature.degreesFahrenheit 60
            , summer = Temperature.degreesFahrenheit 80
            , fall = Temperature.degreesFahrenheit 60
            , adjust = 0
            }
          , { name = "Tropical"
            , winter = Temperature.degreesFahrenheit 50
            , spring = Temperature.degreesFahrenheit 75
            , summer = Temperature.degreesFahrenheit 95
            , fall = Temperature.degreesFahrenheit 75
            , adjust = 1
            }
          ]
            |> List.map
                (\baseline ->
                    Html.tr []
                        [ Html.th [] [ Html.text baseline.name ]
                        , temperatureCell model baseline.winter
                        , temperatureCell model baseline.spring
                        , temperatureCell model baseline.summer
                        , temperatureCell model baseline.fall
                        , Html.td []
                            [ Html.text
                                (if baseline.adjust < 0 then
                                    "Decrease frequency and intensity by one step"

                                 else if baseline.adjust == 0 then
                                    "---"

                                 else
                                    "Increase frequency and intensity by one step"
                                )
                            ]
                        ]
                )
            |> (::)
                (Html.tr []
                    [ Html.th [] [ Html.text "Climate" ]
                    , Html.th [] [ Html.text "Winter Temp." ]
                    , Html.th [] [ Html.text "Spring Temp." ]
                    , Html.th [] [ Html.text "Summer Temp." ]
                    , Html.th [] [ Html.text "Fall Temp." ]
                    , Html.th [] [ Html.text "Precipitation Adjustment" ]
                    ]
                )
            |> Html.table []
        ]


temperatureCell : Model -> Temperature -> Html msg
temperatureCell model temperature =
    let
        label : String
        label =
            case model.unit of
                Fahrenheit ->
                    String.fromInt (round (Temperature.inDegreesFahrenheit temperature)) ++ "° F"

                Celsius ->
                    String.fromInt (round (Temperature.inDegreesCelsius temperature)) ++ "° C"

        ( textColor, backgroundColor ) =
            temperatureColor temperature
    in
    Html.td
        [ Html.Attributes.style "color" (Color.toCssString textColor)
        , Html.Attributes.style "background" (Color.toCssString backgroundColor)
        ]
        [ Html.text label ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Unit unit ->
            { model | unit = unit }


{-| Source: <https://en.wikipedia.org/w/index.php?title=Module:Weather_box/colors&action=edit>
-}
temperatureColor : Temperature -> ( Color, Color )
temperatureColor temperature =
    let
        val : Float
        val =
            Temperature.inDegreesCelsius temperature

        red : Float
        red =
            if val < 4.5 then
                rescale val -42.75 4.5

            else
                rescale val 60 41.5

        green : Float
        green =
            if val <= 4.5 then
                rescale val -42.75 4.5

            else
                rescale val 41.5 4.5

        blue : Float
        blue =
            if val < -42.78 then
                rescale val -90 -42.78

            else
                rescale val 23 4.5

        textColor : Color
        textColor =
            if val < -23.3 || val >= 37.8 then
                Color.white

            else
                Color.black
    in
    ( textColor, Color.rgb red green blue )


{-| Source: <https://en.wikipedia.org/w/index.php?title=Module:Weather_box/colors&action=edit>
-}
rescale : Float -> Float -> Float -> Float
rescale value start stop =
    ((value - start) / (stop - start))
        |> clamp 0 1
