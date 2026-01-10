module Main exposing (main)

import Browser
import Climate exposing (Climate(..))
import Color
import Html exposing (Html)
import Html.Attributes
import Season exposing (Season(..))
import Temperature exposing (Temperature)
import Theme


type alias Model =
    { unit : TemperatureUnit
    , climate : Climate
    , season : Season
    }


type TemperatureUnit
    = Celsius
    | Fahrenheit


type Msg
    = Unit TemperatureUnit
    | ClickedBaseline Climate Season
    | ClickedSeason Season
    | ClickedClimate Climate


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { unit = Celsius
    , climate = Temperate
    , season = Spring
    }


view : Model -> Html Msg
view model =
    Html.main_
        [ Html.Attributes.style "padding" "8px" ]
        [ [ Cold, Temperate, Tropical ]
            |> List.map
                (\climate ->
                    Html.tr []
                        [ Html.td []
                            [ Theme.toggle
                                [ Html.Attributes.style "width" "100%"
                                ]
                                { label = Climate.toString climate
                                , onPress = ClickedClimate climate
                                , selected = model.climate == climate
                                }
                            ]
                        , temperatureCell model climate Winter
                        , temperatureCell model climate Spring
                        , temperatureCell model climate Summer
                        , temperatureCell model climate Fall
                        , Html.td []
                            [ Html.text
                                (if Climate.adjustPrecipitation climate < 0 then
                                    "Decrease"

                                 else if Climate.adjustPrecipitation climate == 0 then
                                    "---"

                                 else
                                    "Increase"
                                )
                            ]
                        ]
                )
            |> (::)
                (let
                    seasonCell : Season -> Html Msg
                    seasonCell season =
                        Html.td []
                            [ Theme.toggle
                                [ Html.Attributes.style "width" "100%"
                                ]
                                { label = Season.toString season
                                , onPress = ClickedSeason season
                                , selected = model.season == season
                                }
                            ]
                 in
                 Html.tr []
                    [ Html.td [] [ Html.text "Climate" ]
                    , seasonCell Winter
                    , seasonCell Spring
                    , seasonCell Summer
                    , seasonCell Fall
                    , Html.td [] [ Html.text "Precipitation" ]
                    ]
                )
            |> Html.table []
        ]


temperatureCell : Model -> Climate -> Season -> Html Msg
temperatureCell model climate season =
    let
        temperature : Temperature
        temperature =
            Climate.averageTemperature climate season

        label : String
        label =
            case model.unit of
                Fahrenheit ->
                    String.fromInt (round (Temperature.inDegreesFahrenheit temperature)) ++ "° F"

                Celsius ->
                    String.fromInt (round (Temperature.inDegreesCelsius temperature)) ++ "° C"

        ( textColor, backgroundColor ) =
            Theme.temperatureColor temperature
    in
    Html.td []
        [ Theme.toggle
            [ Html.Attributes.style "width" "100%"
            , Html.Attributes.style "color" (Color.toCssString textColor)
            , Html.Attributes.style "background" (Color.toCssString backgroundColor)
            ]
            { label = label
            , onPress = ClickedBaseline climate season
            , selected = model.climate == climate && model.season == season
            }
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Unit unit ->
            { model | unit = unit }

        ClickedBaseline climate season ->
            { model
                | climate = climate
                , season = season
            }

        ClickedSeason season ->
            { model | season = season }

        ClickedClimate climate ->
            { model | climate = climate }
