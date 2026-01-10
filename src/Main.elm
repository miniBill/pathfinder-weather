module Main exposing (main)

import Altitude exposing (Altitude(..))
import Baseline
import Browser
import Climate exposing (Climate(..))
import Html exposing (Html)
import Html.Attributes
import Length exposing (Length)
import Season exposing (Season(..))
import Temperature exposing (Temperature)
import Theme


type alias Model =
    { temperatureUnit : TemperatureUnit
    , measureUnit : MeasureUnit
    , climate : Climate
    , season : Season
    , altitude : Altitude
    }


type TemperatureUnit
    = Celsius
    | Fahrenheit


type MeasureUnit
    = Foot
    | Meter


type Msg
    = TemperatureUnit TemperatureUnit
    | MeasureUnit MeasureUnit
    | ClickedBaseline Climate Season
    | ClickedSeason Season
    | ClickedClimate Climate
    | ClickedAltitude Altitude


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { temperatureUnit = Celsius
    , measureUnit = Meter
    , climate = Temperate
    , season = Spring
    , altitude = Lowland
    }


view : Model -> Html Msg
view model =
    Html.main_
        [ Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "8px"
        ]
        [ boxxxy "Select an altitude"
            [ [ ( SeaLevel, "Below " ++ altitudeToString model (Length.feet 1000) )
              , ( Lowland, altitudeToString model (Length.feet 1000) ++ " to " ++ altitudeToString model (Length.feet 5000) )
              , ( Highland, "Above " ++ altitudeToString model (Length.feet 5000) )
              ]
                |> List.map
                    (\( altitude, label ) ->
                        Html.tr []
                            [ Html.td []
                                [ Theme.toggle [ Html.Attributes.style "width" "100%" ]
                                    { color = Nothing
                                    , background = Nothing
                                    , selected = model.altitude == altitude
                                    , label = Altitude.toString altitude
                                    , onPress = ClickedAltitude altitude
                                    }
                                ]
                            , Html.td []
                                [ Theme.toggle [ Html.Attributes.style "width" "100%" ]
                                    { color = Nothing
                                    , background = Nothing
                                    , selected = model.altitude == altitude
                                    , label = label
                                    , onPress = ClickedAltitude altitude
                                    }
                                ]
                            ]
                    )
                |> (::)
                    (Html.tr []
                        [ Html.td [] [ Html.text "Altitude" ]
                        , Html.td [] [ Html.text "Range" ]
                        ]
                    )
                |> Html.table []
            ]
        , boxxxy "Select a baseline"
            [ [ Cold, Temperate, Tropical ]
                |> List.map
                    (\climate ->
                        Html.tr []
                            [ Html.td []
                                [ Theme.toggle
                                    [ Html.Attributes.style "width" "100%"
                                    ]
                                    { color = Nothing
                                    , background = Nothing
                                    , label = Climate.toString climate
                                    , onPress = ClickedClimate climate
                                    , selected = model.climate == climate
                                    }
                                ]
                            , temperatureCell model climate Winter
                            , temperatureCell model climate Spring
                            , temperatureCell model climate Summer
                            , temperatureCell model climate Fall
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
                                    { color = Nothing
                                    , background = Nothing
                                    , label = Season.toString season
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
                        ]
                    )
                |> Html.table []
            ]
        ]


boxxxy : String -> List (Html Msg) -> Html Msg
boxxxy label children =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "start"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.div
            [ Html.Attributes.style "transform" "translate(0,1px)"
            , Html.Attributes.style "border-width" "1px 1px 0 1px"
            , Html.Attributes.style "border-style" "solid"
            , Html.Attributes.style "border-color" "black"
            , Html.Attributes.style "background" "white"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "border-radius" "8px 8px 0 0"
            ]
            [ Html.text label ]
        , Html.div
            [ Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "border-radius" "0 8px 8px 8px"
            ]
            children
        ]


altitudeToString : Model -> Length -> String
altitudeToString model value =
    case model.measureUnit of
        Foot ->
            String.fromInt (round (Length.inFeet value)) ++ " ft."

        Meter ->
            String.fromInt (round (Length.inMeters value) // 100 * 100) ++ " m"


temperatureCell : Model -> Climate -> Season -> Html Msg
temperatureCell model climate season =
    let
        temperature : Temperature
        temperature =
            Baseline.averageTemperature
                { climate = climate
                , season = season
                , altitude = model.altitude
                }

        label : String
        label =
            case model.temperatureUnit of
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
            ]
            { color = Just textColor
            , background = Just backgroundColor
            , label = label
            , onPress = ClickedBaseline climate season
            , selected = model.climate == climate && model.season == season
            }
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedBaseline climate season ->
            { model
                | climate = climate
                , season = season
            }

        ClickedSeason season ->
            { model | season = season }

        ClickedClimate climate ->
            { model | climate = climate }

        ClickedAltitude altitude ->
            { model | altitude = altitude }

        TemperatureUnit temperatureUnit ->
            { model | temperatureUnit = temperatureUnit }

        MeasureUnit measureUnit ->
            { model | measureUnit = measureUnit }
