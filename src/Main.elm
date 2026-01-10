module Main exposing (main)

import Altitude exposing (Altitude(..), Highland(..))
import Baseline
import Browser
import Climate exposing (Climate(..), Cold(..))
import Color
import Color.Oklch as Oklch
import Frequency
import Html exposing (Attribute, Html)
import Html.Attributes
import Intensity
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
        , Html.Attributes.style "flex-wrap" "wrap"
        , Html.Attributes.style "gap" "8px"
        ]
        [ boxxxy "Select your units"
            [ Html.table
                []
                [ Html.tr []
                    [ Html.td []
                        [ Theme.toggle
                            [ Html.Attributes.style "width" "100%" ]
                            { label = "Celsius"
                            , onPress = TemperatureUnit Celsius
                            , selected = model.temperatureUnit == Celsius
                            , background = Nothing
                            , color = Nothing
                            }
                        ]
                    , Html.td []
                        [ Theme.toggle
                            [ Html.Attributes.style "width" "100%" ]
                            { label = "Fahrenheit"
                            , onPress = TemperatureUnit Fahrenheit
                            , selected = model.temperatureUnit == Fahrenheit
                            , background = Nothing
                            , color = Nothing
                            }
                        ]
                    ]
                , Html.tr []
                    [ Html.td []
                        [ Theme.toggle
                            [ Html.Attributes.style "width" "100%" ]
                            { label = "Meter"
                            , onPress = MeasureUnit Meter
                            , selected = model.measureUnit == Meter
                            , background = Nothing
                            , color = Nothing
                            }
                        ]
                    , Html.td []
                        [ Theme.toggle
                            [ Html.Attributes.style "width" "100%" ]
                            { label = "Foot"
                            , onPress = MeasureUnit Foot
                            , selected = model.measureUnit == Foot
                            , background = Nothing
                            , color = Nothing
                            }
                        ]
                    ]
                ]
            ]
        , let
            altitudeCell : List (Attribute Msg) -> Altitude -> String -> Html Msg
            altitudeCell attrs altitude label =
                Html.td attrs
                    [ Theme.toggle
                        [ Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        ]
                        { color = Nothing
                        , background = Nothing
                        , selected = model.altitude == altitude
                        , label = label
                        , onPress = ClickedAltitude altitude
                        }
                    ]
          in
          boxxxy "Select an altitude"
            [ Html.table
                [ Html.Attributes.style "display" "grid"
                , Html.Attributes.style "grid-template-columns" "auto auto auto"
                ]
                [ Html.td [] [ Html.text "Altitude" ]
                , Html.td [ Html.Attributes.style "grid-column" "span 2" ] [ Html.text "Range" ]
                , altitudeCell [] SeaLevel (Altitude.toString SeaLevel)
                , altitudeCell [ Html.Attributes.style "grid-column" "span 2" ] SeaLevel ("Below " ++ altitudeToString model (Length.feet 1000))
                , altitudeCell [] Lowland (Altitude.toString Lowland)
                , altitudeCell [ Html.Attributes.style "grid-column" "span 2" ] Lowland (altitudeToString model (Length.feet 1000) ++ " to " ++ altitudeToString model (Length.feet 5000))
                , altitudeCell [ Html.Attributes.style "grid-row" "span 3" ] (Highland Altitude.Regular) (Altitude.toString (Highland Altitude.Regular))
                , altitudeCell [ Html.Attributes.style "grid-row" "span 3" ] (Highland Altitude.Regular) ("Above " ++ altitudeToString model (Length.feet 5000))
                , altitudeCell [] (Highland Altitude.Regular) "Regular"
                , altitudeCell [] (Highland Arid) "Arid and flat"
                , altitudeCell [] (Highland HighAltitude) ("Above " ++ altitudeToString model (Length.feet 10000))
                ]
            ]
        , boxxxy "Select a baseline"
            [ [ ( Tropical, 0, 30 )
              , ( Temperate, 30, 60 )
              , ( Cold Climate.Regular, 60, 82 )
              , ( Cold Arctic, 82, 86 )
              , ( Cold Polar, 86, 90 )
              ]
                |> List.map
                    (\( climate, minLatitude, maxLatitude ) ->
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
                            , Html.td [ Html.Attributes.style "text-align" "right" ] [ Html.text (String.fromInt minLatitude ++ "°") ]
                            , Html.td [] [ Html.text "–" ]
                            , Html.td [] [ Html.text (String.fromInt maxLatitude ++ "°") ]
                            , baselineCell model climate Winter
                            , baselineCell model climate Spring
                            , baselineCell model climate Summer
                            , baselineCell model climate Fall
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
                        , Html.td [ Html.Attributes.colspan 3 ] [ Html.text "Latitude" ]
                        , seasonCell Winter
                        , seasonCell Spring
                        , seasonCell Summer
                        , seasonCell Fall
                        ]
                    )
                |> Html.table [ Html.Attributes.style "white-space" "nowrap" ]
            ]
        , boxxxy "Baseline climate"
            [ Html.table []
                [ Html.tr []
                    [ Html.td [] [ Html.text "Day temperature" ]
                    , temperatureCell model
                        [ Html.Attributes.colspan 3 ]
                        (Baseline.averageTemperature model)
                    ]
                , Html.tr []
                    [ Html.td []
                        [ Html.text "Night temperature" ]
                    , temperatureCell model
                        []
                        (Baseline.averageTemperature model
                            |> Temperature.plus (Temperature.fahrenheitDegrees -15)
                        )
                    , Html.td [] [ Html.text "to" ]
                    , temperatureCell model
                        []
                        (Baseline.averageTemperature model
                            |> Temperature.plus (Temperature.fahrenheitDegrees -5)
                        )
                    ]
                , Html.tr []
                    [ Html.td [] [ Html.text "Average precipitation frequency" ]
                    , Html.td
                        [ Html.Attributes.style "border" "2px solid black"
                        , Html.Attributes.colspan 3
                        ]
                        [ Html.text (Frequency.toString (Baseline.precipitationFrequency model)) ]
                    ]
                , Html.tr []
                    [ Html.td [] [ Html.text "Average precipitation intensity" ]
                    , Html.td
                        [ Html.Attributes.style "border" "2px solid black"
                        , Html.Attributes.colspan 3
                        ]
                        [ Html.text (Intensity.toString (Baseline.precipitationIntensity model)) ]
                    ]
                ]
            ]
        ]


temperatureToString : Model -> Temperature -> String
temperatureToString model temperature =
    let
        format : (Temperature -> Float) -> String -> String
        format f l =
            let
                rounded : Int
                rounded =
                    round (f temperature)
            in
            if rounded < 0 then
                "−" ++ String.fromInt -rounded ++ "° " ++ l

            else
                String.fromInt rounded ++ "° " ++ l
    in
    case model.temperatureUnit of
        Fahrenheit ->
            format Temperature.inDegreesFahrenheit "F"

        Celsius ->
            format Temperature.inDegreesCelsius "C"


boxxxy : String -> List (Html Msg) -> Html Msg
boxxxy label children =
    let
        backgroundColor : String
        backgroundColor =
            Oklch.toCssString (Oklch.oklch 0.9 0.04 0.75)
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "transform" "translate(0,1px)"
            ]
            [ Html.div
                [ Html.Attributes.style "border-width" "1px 1px 0 1px"
                , Html.Attributes.style "border-style" "solid"
                , Html.Attributes.style "border-color" "black"
                , Html.Attributes.style "background" backgroundColor
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "border-radius" "8px 8px 0 0"
                , Html.Attributes.style "align-self" "start"
                ]
                [ Html.text label ]
            , Html.div
                [ Html.Attributes.style "width" "8px"
                ]
                []
            ]
        , Html.div
            [ Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "border-radius" "0 8px 8px 8px"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "background" backgroundColor
            ]
            children
        ]


altitudeToString : Model -> Length -> String
altitudeToString model value =
    case model.measureUnit of
        Foot ->
            String.fromInt (round (Length.inFeet value)) ++ " ft."

        Meter ->
            String.fromInt (round (Length.inMeters value) // 100 * 100) ++ "m"


temperatureCell : Model -> List (Attribute msg) -> Temperature -> Html msg
temperatureCell model attrs temperature =
    let
        label : String
        label =
            temperatureToString model temperature

        ( textColor, backgroundColor, borderColor ) =
            Theme.temperatureColor temperature
    in
    Html.td
        ([ Html.Attributes.style "color" (Color.toCssString textColor)
         , Html.Attributes.style "background" (Color.toCssString backgroundColor)
         , Html.Attributes.style "border" ("2px solid " ++ Color.toCssString borderColor)
         , Html.Attributes.style "white-space" "nowrap"
         ]
            ++ attrs
        )
        [ Html.text label ]


baselineCell : Model -> Climate -> Season -> Html Msg
baselineCell model climate season =
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
            temperatureToString model temperature

        ( textColor, backgroundColor, borderColor ) =
            Theme.temperatureColor temperature
    in
    Html.td []
        [ Theme.toggle
            [ Html.Attributes.style "width" "100%"
            , Html.Attributes.style "border" ("2px solid " ++ Color.toCssString borderColor)
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
