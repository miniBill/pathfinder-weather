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
import Html.Events
import Intensity exposing (Intensity(..))
import Length exposing (Length)
import List.Extra
import Precipitation exposing (Precipitation(..))
import Quantity
import Season exposing (Season(..))
import Speed exposing (Speed)
import Temperature exposing (Temperature)
import Theme


type alias Model =
    { temperatureUnit : TemperatureUnit
    , measureUnit : MeasureUnit
    , speedUnit : SpeedUnit
    , climate : Climate
    , season : Season
    , altitude : Altitude
    , currentTemperature : TemperatureRange
    , isDesert : Bool
    }


type TemperatureRange
    = Below32
    | Between32And40
    | Above40


type TemperatureUnit
    = Celsius
    | Fahrenheit


type MeasureUnit
    = Meter
    | Foot


type SpeedUnit
    = KilometersPerHour
    | MilesPerHour
    | Knots


type Msg
    = TemperatureUnit TemperatureUnit
    | MeasureUnit MeasureUnit
    | SpeedUnit SpeedUnit
    | ClickedBaseline Climate Season
    | ClickedSeason Season
    | ClickedClimate Climate
    | ClickedAltitude Altitude
    | ClickedIsDesert Bool
    | ClickedCurrentTemperature TemperatureRange


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
    , speedUnit = KilometersPerHour
    , climate = Temperate
    , season = Spring
    , altitude = Lowland
    , currentTemperature = Above40
    , isDesert = False
    }


view : Model -> Html Msg
view model =
    Html.main_
        [ Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-wrap" "wrap"
        , Html.Attributes.style "gap" "8px"
        ]
        [ unitsBox model
        , altitudeBox model
        , baselineInputBox model
        , baselineOutputBox model
        , variationsBox model
        , currentTemperatureBox model
        , resultingPrecipitation model
        , thunderstormBox model
        , windBox model
        ]


type alias Row =
    { percentMin : Int
    , percentMax : Int
    , precipitation : Precipitation
    , duration : String
    }


resultingPrecipitation : Model -> Html Msg
resultingPrecipitation model =
    let
        ( _, probability ) =
            Frequency.toString (Baseline.precipitationFrequency model)

        row : Row -> Html msg
        row { percentMin, percentMax, precipitation, duration } =
            percentRow ( percentMin, percentMax )
                [ Html.td [] [ Html.text (Precipitation.toString precipitation) ]
                , Html.td []
                    [ if duration == "1" then
                        Html.text (duration ++ " hour")

                      else
                        Html.text (duration ++ " hours")
                    ]
                ]

        header : Html msg
        header =
            Html.tr []
                [ Html.th [ Html.Attributes.colspan 3 ] [ Html.text "d%" ]
                , Html.th [] [ Html.text "Precipitation" ]
                , Html.th [] [ Html.text "Duration" ]
                ]

        data : List Row
        data =
            precipitationData model
    in
    boxxxy "Precipitation table"
        []
        [ Html.p []
            [ Html.text ("Roll the daily probability (" ++ String.fromInt probability ++ "%).")
            ]
        , Html.p []
            [ Html.text "If there is precipitation, roll a random starting hour."
            ]
        , Html.table [] (header :: List.map row data)
        , data
            |> List.map .precipitation
            |> List.Extra.unique
            |> List.map
                (\precipitation ->
                    Html.p []
                        [ Html.b [] [ Html.text (Precipitation.toString precipitation ++ ": ") ]
                        , Html.text (Precipitation.description precipitation)
                        ]
                )
            |> Html.div []
        ]


precipitationData : Model -> List Row
precipitationData model =
    let
        maybeSleet : Precipitation -> Precipitation
        maybeSleet precipitation =
            if model.currentTemperature == Above40 then
                precipitation

            else
                Sleet

        intensity : Intensity
        intensity =
            Baseline.precipitationIntensity model
    in
    case ( intensity, model.currentTemperature /= Below32 ) of
        ( Light, True ) ->
            [ Row 1 20 LightFog "1d8"
            , Row 21 40 MediumFog "1d6"
            , Row 41 50 Drizzle "1d4"
            , Row 51 75 Drizzle "2d12"
            , Row 76 90 LightRain "1d4"
            , Row 91 100 (maybeSleet LightRain) "1"
            ]

        ( Light, False ) ->
            [ Row 1 20 LightFog "1d6"
            , Row 21 40 LightFog "1d8"
            , Row 41 50 MediumFog "1d4"
            , Row 51 60 LightSnow "1"
            , Row 61 75 LightSnow "1d4"
            , Row 76 100 LightSnow "2d12"
            ]

        ( Medium, True ) ->
            [ Row 1 10 MediumFog "1d8"
            , Row 11 20 MediumFog "1d12"
            , Row 21 30 HeavyFog "1d4"
            , Row 31 35 Rain "1d4"
            , Row 36 70 Rain "1d8"
            , Row 71 90 Rain "2d12"
            , Row 91 100 (maybeSleet Rain) "1d4"
            ]

        ( Medium, False ) ->
            [ Row 1 10 MediumFog "1d6"
            , Row 11 20 MediumFog "1d8"
            , Row 21 30 HeavyFog "1d4"
            , Row 31 50 MediumSnow "1d4"
            , Row 51 90 MediumSnow "1d8"
            , Row 91 100 MediumSnow "2d12"
            ]

        ( Heavy, True ) ->
            [ Row 1 10 HeavyFog "1d8"
            , Row 11 20 HeavyFog "2d6"
            , Row 21 50 HeavyRain "1d12"
            , Row 51 70 HeavyRain "2d12"
            , Row 71 85 (maybeSleet HeavyRain) "1d8"
            , Row 86 90 Thunderstorm "1"
            , Row 91 100 Thunderstorm "1d3"
            ]

        ( Heavy, False ) ->
            [ Row 1 10 MediumFog "1d8"
            , Row 11 20 HeavyFog "2d6"
            , Row 21 60 LightSnow "2d12"
            , Row 61 90 MediumSnow "1d8"
            , Row 91 100 HeavySnow "1d6"
            ]

        ( Torrential, True ) ->
            [ Row 1 5 HeavyFog "1d8"
            , Row 6 10 HeavyFog "2d6"
            , Row 11 30 HeavyRain "2d6"
            , Row 31 60 HeavyRain "2d12"
            , Row 61 80 (maybeSleet HeavyRain) "2d6"
            , Row 81 95 Thunderstorm "1d3"
            , Row 96 100 Thunderstorm "1d6"
            ]

        ( Torrential, False ) ->
            [ Row 1 5 HeavyFog "1d8"
            , Row 6 10 HeavyFog "2d6"
            , Row 11 50 HeavySnow "1d4"
            , Row 51 90 HeavySnow "1d8"
            , Row 91 100 HeavySnow "2d12"
            ]


thunderstormBox : Model -> Html Msg
thunderstormBox model =
    let
        row : { percentMin : Int, percentMax : Int, strength : String } -> Html msg
        row { percentMin, percentMax, strength } =
            percentRow ( percentMin, percentMax ) [ Html.td [] [ Html.text strength ] ]

        data : List Row
        data =
            precipitationData model
    in
    if List.any (\dataRow -> dataRow.precipitation == Thunderstorm) data then
        let
            header : Html msg
            header =
                Html.tr []
                    [ Html.th [ Html.Attributes.colspan 3 ] [ Html.text "d%" ]
                    , Html.th [] [ Html.text "Wind strength" ]
                    ]

            rows : List (Html msg)
            rows =
                [ row { percentMin = 1, percentMax = 50, strength = "Strong winds" }
                , row { percentMin = 51, percentMax = 90, strength = "Severe winds" }
                , row { percentMin = 91, percentMax = 100, strength = "Windstorm" }
                ]
        in
        boxxxy "Thunderstorms"
            []
            [ Html.table [] (header :: rows)
            , Html.p []
                [ Html.text "In addition, there is a 40% chance that a thunderstorm features hail either up to an hour before or during the storm. An even greater danger presented by a thunderstorm is the lightning that occurs during the storm. These electrical discharges, generated by the roiling butts, can pose a hazard to creatures that do not have proper shelters, especially creatures clad in metal armor. Every 10 minutes during a thunderstorm, a bolt of lightning strikes an unsheltered creature at random (though this can strike wildlife as easily as PCs). A creature struck by this lightning must succeed a DC 18 Reflex saving throw or take 10d8 points of electricity damage (a successful saving throw halves the damage). Creatures in metal armor take a –4 penalty on the Reflex saving throw."
                ]
            , Html.p []
                [ Html.text
                    ("There is a 10% chance that a thunderstorm with winds of windstorm strength also generates a tornado, while thunderstorms with windstorm-strength winds in temperatures higher than "
                        ++ temperatureToString model (Temperature.degreesFahrenheit 85)
                        ++ " also have a 20% chance of being a precursor to a hurricane. There is a 20% chance that a thunderstorm of any strength in the desert also generates a haboob."
                    )
                ]
            ]

    else
        Html.text ""


windBox : Model -> Html Msg
windBox model =
    let
        windSpeedCells : Float -> Float -> List (Html msg)
        windSpeedCells from to =
            [ Html.td
                [ Html.Attributes.style "text-align" "right" ]
                [ Html.text
                    (speedToString
                        (if from == 0 then
                            0

                         else
                            1
                        )
                        (Speed.milesPerHour from)
                    )
                ]
            , Html.td [ Html.Attributes.style "text-align" "center" ] [ Html.text "–" ]
            , Html.td [] [ Html.text (speedToString 0 (Speed.milesPerHour to)) ]
            , Html.td [] [ Html.text unitString ]
            ]

        speedToString : Int -> Speed -> String
        speedToString adjust speed =
            let
                format : Float -> String
                format v =
                    String.fromInt (adjust + round v)
            in
            case model.speedUnit of
                KilometersPerHour ->
                    format (Speed.inKilometersPerHour speed)

                MilesPerHour ->
                    format (Speed.inMilesPerHour speed)

                Knots ->
                    format (Speed.inKilometersPerHour speed / 1.852)

        unitString : String
        unitString =
            case model.speedUnit of
                KilometersPerHour ->
                    "km/h"

                MilesPerHour ->
                    "mph"

                Knots ->
                    "kt"
    in
    boxxxy "Wind"
        [ Html.Attributes.style "max-width" "600px" ]
        [ Html.p [] [ Html.text "Ignore this box if fog or thunderstorm." ]
        , Html.table []
            [ Html.tr []
                [ Html.th [ Html.Attributes.style "inline-size" "min-content", Html.Attributes.colspan 3 ] [ Html.text "d%" ]
                , Html.th [ Html.Attributes.style "inline-size" "min-content" ] [ Html.text "Wind Strength" ]
                , Html.th [ Html.Attributes.style "inline-size" "min-content", Html.Attributes.colspan 4 ] [ Html.text "Wind Speed" ]
                , Html.th [ Html.Attributes.style "inline-size" "min-content" ] [ Html.text "Ranged Weapon Penalty" ]
                , Html.th [ Html.Attributes.style "inline-size" "min-content" ] [ Html.text "Check Size" ]
                , Html.th [ Html.Attributes.style "inline-size" "min-content" ] [ Html.text "Blown Away Size" ]
                , Html.th [ Html.Attributes.style "inline-size" "min-content" ] [ Html.text "Skill Penalty" ]
                ]
            , percentRow ( 1, 50 )
                (Html.td [] [ Html.text "Light" ]
                    :: windSpeedCells 0 10
                    ++ [ Html.td [] []
                       , Html.td [] []
                       , Html.td [] []
                       , Html.td [] []
                       ]
                )
            , percentRow ( 51, 80 )
                (Html.td [] [ Html.text "Moderate" ]
                    :: windSpeedCells 10 20
                    ++ [ Html.td [] []
                       , Html.td [] []
                       , Html.td [] []
                       , Html.td [] []
                       ]
                )
            , percentRow ( 81, 90 )
                (Html.td [] [ Html.text "Strong" ]
                    :: windSpeedCells 20 30
                    ++ [ Html.td [ Html.Attributes.style "text-align" "center" ] [ Html.text "–2" ]
                       , Html.td [] [ Html.text "Tiny" ]
                       , Html.td [] []
                       , Html.td [ Html.Attributes.style "text-align" "center" ] [ Html.text "–2" ]
                       ]
                )
            , percentRow ( 91, 95 )
                (Html.td [] [ Html.text "Severe" ]
                    :: windSpeedCells 30 50
                    ++ [ Html.td [ Html.Attributes.style "text-align" "center" ] [ Html.text "–4" ]
                       , Html.td [] [ Html.text "Small" ]
                       , Html.td [] [ Html.text "Tiny" ]
                       , Html.td [ Html.Attributes.style "text-align" "center" ] [ Html.text "–4" ]
                       ]
                )
            , percentRow ( 96, 100 )
                (Html.td [] [ Html.text "Windstorm" ]
                    :: windSpeedCells 50 74
                    ++ [ Html.td [] [ Html.text "Impossible" ]
                       , Html.td [] [ Html.text "Medium" ]
                       , Html.td [] [ Html.text "Small" ]
                       , Html.td [ Html.Attributes.style "text-align" "center" ] [ Html.text "–8" ]
                       ]
                )
            , Html.tr []
                ([ Html.td
                    [ Html.Attributes.colspan 3
                    , Html.Attributes.style "text-align" "center"
                    ]
                    [ Html.text "—" ]
                 , Html.td [] [ Html.text "Hurricane" ]
                 ]
                    ++ windSpeedCells 74 174
                    ++ [ Html.td [] [ Html.text "Impossible" ]
                       , Html.td [] [ Html.text "Large" ]
                       , Html.td [] [ Html.text "Medium" ]
                       , Html.td [ Html.Attributes.style "text-align" "center" ] [ Html.text "–12" ]
                       ]
                )
            , Html.tr []
                ([ Html.td
                    [ Html.Attributes.colspan 3
                    , Html.Attributes.style "text-align" "center"
                    ]
                    [ Html.text "—" ]
                 , Html.td [] [ Html.text "Tornado" ]
                 ]
                    ++ windSpeedCells 174 300
                    ++ [ Html.td [] [ Html.text "Impossible" ]
                       , Html.td [] [ Html.text "Huge" ]
                       , Html.td [] [ Html.text "Large*" ]
                       , Html.td [ Html.Attributes.style "text-align" "center" ] [ Html.text "–16" ]
                       ]
                )
            ]
        , Html.p [] [ Html.b [] [ Html.text "Wind Speed: " ], Html.text "Wind speed typically fluctuates between these values through the period of the day. For moderate or higher wind strength there are periods in the day when the wind speed dips below the listed range." ]
        , Html.p [] [ Html.b [] [ Html.text "Ranged Weapon Penalty: " ], Html.text "These are the penalties that characters take when firing ranged weapons. This includes ranged attacks made via conjuration, but not evocation." ]
        , Html.p [] [ Html.b [] [ Html.text "Check Size: " ], Html.text "Creatures of the listed size need a DC 10 Strength check (on the ground) or DC 20 Fly check to move against the wind." ]
        , Html.p [] [ Html.b [] [ Html.text "Blown Away Size: " ], Html.text "Creatures of the listed size on the ground are knocked prone, roll 1d4×10 feet, and take 2d6 points of nonlethal damage, unless they succeed on a DC 15 Strength check. Flying creatures of the listed size are blown back 2d6×10 feet and take 2d6 points of nonlethal damage due to battering and buffeting, unless they succeed at a DC 25 Fly check." ]
        , Html.p [] [ Html.b [] [ Html.text "Skill Penalty: " ], Html.text "These penalties apply on Fly and sound-based Perception, GMs may apply them on Acrobatics, Climb, or any other affected by wind speed." ]
        ]


percentRow : ( Int, Int ) -> List (Html msg) -> Html msg
percentRow ( percentMin, percentMax ) cells =
    Html.tr []
        ([ Html.td
            [ Html.Attributes.style "text-align" "right" ]
            [ Html.text (String.fromInt percentMin) ]
         , Html.td [ Html.Attributes.style "text-align" "center" ] [ Html.text "–" ]
         , Html.td [] [ Html.text (String.fromInt percentMax) ]
         ]
            ++ cells
        )


variationsBox : Model -> Html Msg
variationsBox model =
    let
        header : Html msg
        header =
            Html.tr []
                [ Html.th [ Html.Attributes.colspan 3 ] [ Html.text "d%" ]
                , Html.th [] [ Html.text "Variation" ]
                , Html.th [] [ Html.text "Duration" ]
                ]

        var : Int -> Int -> Int -> String -> Html msg
        var pctMin pctMax dice duration =
            let
                variation : String
                variation =
                    if dice == 0 then
                        "No variation"

                    else if dice < 0 then
                        String.fromInt dice ++ "d" ++ deltaToString model (Temperature.fahrenheitDegrees 10)

                    else
                        "+" ++ String.fromInt dice ++ "d" ++ deltaToString model (Temperature.fahrenheitDegrees 10)
            in
            if pctMin == pctMax then
                Html.tr []
                    [ Html.td
                        [ Html.Attributes.style "text-align" "center"
                        , Html.Attributes.colspan 3
                        ]
                        [ Html.text (String.fromInt pctMin) ]
                    , Html.td [] [ Html.text variation ]
                    , Html.td [] [ Html.text (duration ++ " days") ]
                    ]

            else
                percentRow ( pctMin, pctMax )
                    [ Html.td [] [ Html.text variation ]
                    , Html.td [] [ Html.text (duration ++ " days") ]
                    ]

        rows : List (Html msg)
        rows =
            case model.climate of
                Cold _ ->
                    [ var 1 20 -3 "1d4"
                    , var 21 40 -2 "1d6+1"
                    , var 41 60 -1 "1d6+2"
                    , var 61 80 0 "1d6+2"
                    , var 81 95 1 "1d6+1"
                    , var 96 99 2 "1d4"
                    , var 100 100 3 "1d2"
                    ]

                Temperate ->
                    [ var 1 5 -3 "1d2"
                    , var 6 15 -2 "1d4"
                    , var 16 35 -1 "1d4+1"
                    , var 36 65 0 "1d6+1"
                    , var 66 85 1 "1d4+1"
                    , var 86 95 2 "1d4"
                    , var 96 100 3 "1d2"
                    ]

                Tropical ->
                    [ var 1 10 -2 "1d2"
                    , var 11 25 -1 "1d2"
                    , var 26 55 0 "1d4"
                    , var 56 85 1 "1d4"
                    , var 86 100 2 "1d2"
                    ]
    in
    boxxxy "Variations" [] [ Html.table [] (header :: rows) ]


currentTemperatureBox : Model -> Html Msg
currentTemperatureBox model =
    let
        forty : Temperature
        forty =
            Temperature.degreesFahrenheit 40

        thirtyTwo : Temperature
        thirtyTwo =
            Temperature.degreesFahrenheit 32
    in
    boxxxy "Current temperature"
        []
        [ Theme.toggle []
            { onPress = ClickedCurrentTemperature Above40
            , selected = model.currentTemperature == Above40
            , label = "Above " ++ temperatureToString model forty
            , color = Nothing
            , background = Nothing
            }
        , Theme.toggle []
            { onPress = ClickedCurrentTemperature Between32And40
            , selected = model.currentTemperature == Between32And40
            , label = "Between " ++ temperatureToString model thirtyTwo ++ " and " ++ temperatureToString model forty
            , color = Nothing
            , background = Nothing
            }
        , Theme.toggle []
            { onPress = ClickedCurrentTemperature Below32
            , selected = model.currentTemperature == Below32
            , label = "Below " ++ temperatureToString model thirtyTwo
            , color = Nothing
            , background = Nothing
            }
        ]


baselineOutputBox : Model -> Html Msg
baselineOutputBox model =
    let
        ( frequencyString, frequencyPercent ) =
            Frequency.toString (Baseline.precipitationFrequency model)

        header : String -> Html msg
        header label =
            Html.tr []
                [ Html.th
                    [ Html.Attributes.colspan 4 ]
                    [ Html.text label ]
                ]

        row : String -> List (Html msg) -> Html msg
        row label cells =
            Html.tr []
                (Html.td [] [ Html.text label ]
                    :: cells
                )
    in
    boxxxy "Baseline climate"
        []
        [ [ header "Temperature"
          , row "Day"
                [ Baseline.averageTemperature model
                    |> temperatureCell model [ Html.Attributes.colspan 3 ]
                ]
          , row "Night"
                [ Baseline.averageTemperature model
                    |> Temperature.plus (Temperature.fahrenheitDegrees -15)
                    |> temperatureCell model []
                , Html.td [] [ Html.text "to" ]
                , Baseline.averageTemperature model
                    |> Temperature.plus (Temperature.fahrenheitDegrees -5)
                    |> temperatureCell model []
                ]
          , header "Precipitation"
          , row "Intensity"
                [ [ Html.text (Intensity.toString (Baseline.precipitationIntensity model)) ]
                    |> Html.td
                        [ Html.Attributes.style "border" "2px solid black"
                        , Html.Attributes.colspan 3
                        ]
                ]
          , row "Frequency"
                [ [ Html.text frequencyString ]
                    |> Html.td
                        [ Html.Attributes.style "border" "2px solid black"
                        , Html.Attributes.colspan 3
                        ]
                ]
          , row "Probability"
                [ [ Html.text (String.fromInt frequencyPercent ++ "% / day") ]
                    |> Html.td
                        [ Html.Attributes.style "border" "2px solid black"
                        , Html.Attributes.colspan 3
                        ]
                ]
          ]
            |> Html.table []
        ]


baselineInputBox : Model -> Html Msg
baselineInputBox model =
    let
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

        header : Html Msg
        header =
            Html.tr []
                [ Html.td [] [ Html.text "Climate" ]
                , Html.td [ Html.Attributes.colspan 3 ] [ Html.text "Latitude" ]
                , seasonCell Winter
                , seasonCell Spring
                , seasonCell Summer
                , seasonCell Fall
                ]

        baselineRow : Climate -> Int -> Int -> Html Msg
        baselineRow climate minLatitude maxLatitude =
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
                , Html.td
                    [ Html.Attributes.style "text-align" "right" ]
                    [ Html.text (String.fromInt minLatitude ++ "°") ]
                , Html.td [] [ Html.text "–" ]
                , Html.td [] [ Html.text (String.fromInt maxLatitude ++ "°") ]
                , baselineCell model climate Winter
                , baselineCell model climate Spring
                , baselineCell model climate Summer
                , baselineCell model climate Fall
                ]
    in
    boxxxy "Select a baseline"
        []
        [ Html.table
            [ Html.Attributes.style "white-space" "nowrap" ]
            [ header
            , baselineRow Tropical 0 30
            , baselineRow Temperate 30 60
            , baselineRow (Cold Climate.Regular) 60 82
            , baselineRow (Cold Arctic) 82 86
            , baselineRow (Cold Polar) 86 90
            ]
        , Html.label []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked model.isDesert
                , Html.Events.onCheck ClickedIsDesert
                ]
                []
            , Html.text " Is a desert"
            ]
        ]


altitudeBox : Model -> Html Msg
altitudeBox model =
    let
        altitudeCell : List (Attribute Msg) -> Altitude -> String -> Html Msg
        altitudeCell attrs altitude label =
            Html.div attrs
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
        []
        [ Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "auto auto auto"
            , Html.Attributes.style "gap" "4px"
            ]
            [ Html.div [] [ Html.text "Altitude" ]
            , Html.div [ Html.Attributes.style "grid-column" "span 2" ] [ Html.text "Range" ]
            , altitudeCell []
                SeaLevel
                (Altitude.toString SeaLevel)
            , altitudeCell
                [ Html.Attributes.style "grid-column" "span 2" ]
                SeaLevel
                ("Below " ++ altitudeToString model (Length.feet 1000))
            , altitudeCell []
                Lowland
                (Altitude.toString Lowland)
            , altitudeCell
                [ Html.Attributes.style "grid-column" "span 2" ]
                Lowland
                (altitudeToString model (Length.feet 1000)
                    ++ " to "
                    ++ altitudeToString model (Length.feet 5000)
                )
            , altitudeCell
                [ Html.Attributes.style "grid-row" "span 3" ]
                (Highland Altitude.Regular)
                (Altitude.toString (Highland Altitude.Regular))
            , altitudeCell
                [ Html.Attributes.style "grid-row" "span 3" ]
                (Highland Altitude.Regular)
                ("Above " ++ altitudeToString model (Length.feet 5000))
            , altitudeCell []
                (Highland Altitude.Regular)
                "Regular"
            , altitudeCell []
                (Highland Arid)
                "Arid and flat"
            , altitudeCell []
                (Highland HighAltitude)
                ("Above " ++ altitudeToString model (Length.feet 10000))
            ]
        ]


unitsBox : Model -> Html Msg
unitsBox model =
    boxxxy "Select your units"
        []
        [ Html.table
            []
            [ Html.tr []
                [ Html.td [ Html.Attributes.colspan 3 ]
                    [ Theme.toggle
                        [ Html.Attributes.style "width" "100%" ]
                        { label = "Celsius"
                        , onPress = TemperatureUnit Celsius
                        , selected = model.temperatureUnit == Celsius
                        , background = Nothing
                        , color = Nothing
                        }
                    ]
                , Html.td [ Html.Attributes.colspan 3 ]
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
                [ Html.td [ Html.Attributes.colspan 3 ]
                    [ Theme.toggle
                        [ Html.Attributes.style "width" "100%" ]
                        { label = "Meter"
                        , onPress = MeasureUnit Meter
                        , selected = model.measureUnit == Meter
                        , background = Nothing
                        , color = Nothing
                        }
                    ]
                , Html.td [ Html.Attributes.colspan 3 ]
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
            , Html.tr []
                [ Html.td [ Html.Attributes.colspan 2 ]
                    [ Theme.toggle
                        [ Html.Attributes.style "width" "100%" ]
                        { label = "km/h"
                        , onPress = SpeedUnit KilometersPerHour
                        , selected = model.speedUnit == KilometersPerHour
                        , background = Nothing
                        , color = Nothing
                        }
                    ]
                , Html.td [ Html.Attributes.colspan 2 ]
                    [ Theme.toggle
                        [ Html.Attributes.style "width" "100%" ]
                        { label = "mph"
                        , onPress = SpeedUnit MilesPerHour
                        , selected = model.speedUnit == MilesPerHour
                        , background = Nothing
                        , color = Nothing
                        }
                    ]
                , Html.td [ Html.Attributes.colspan 2 ]
                    [ Theme.toggle
                        [ Html.Attributes.style "width" "100%" ]
                        { label = "Knots"
                        , onPress = SpeedUnit Knots
                        , selected = model.speedUnit == Knots
                        , background = Nothing
                        , color = Nothing
                        }
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


deltaToString : Model -> Temperature.Delta -> String
deltaToString model delta =
    let
        format : (Temperature.Delta -> Float) -> String -> String
        format f l =
            let
                rounded : Int
                rounded =
                    round (f delta)
            in
            if rounded < 0 then
                "−" ++ String.fromInt -rounded ++ "° " ++ l

            else
                String.fromInt rounded ++ "° " ++ l
    in
    case model.temperatureUnit of
        Fahrenheit ->
            format Temperature.inFahrenheitDegrees "F"

        Celsius ->
            format Temperature.inCelsiusDegrees "C"


boxxxy : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
boxxxy label attrs children =
    let
        backgroundColor : String
        backgroundColor =
            Oklch.toCssString (Oklch.oklch 0.9 0.04 0.75)
    in
    Html.div
        ([ Html.Attributes.style "display" "flex"
         , Html.Attributes.style "flex-direction" "column"
         , Html.Attributes.style "max-width" "480px"
         ]
            ++ attrs
        )
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
                , Html.Attributes.style "font-weight" "bold"
                ]
                [ Html.text label ]
            , Html.div [ Html.Attributes.style "width" "8px" ] []
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
            String.fromInt (round (Length.inMeters value / 100) * 100) ++ "m"


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
         , Html.Attributes.style "text-align" "center"
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
        TemperatureUnit temperatureUnit ->
            { model | temperatureUnit = temperatureUnit }

        MeasureUnit measureUnit ->
            { model | measureUnit = measureUnit }

        SpeedUnit speedUnit ->
            { model | speedUnit = speedUnit }

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

        ClickedIsDesert isDesert ->
            { model | isDesert = isDesert }

        ClickedCurrentTemperature currentTemperature ->
            { model | currentTemperature = currentTemperature }
