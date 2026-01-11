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
import Season exposing (Season(..))
import Temperature exposing (Temperature)
import Theme


type alias Model =
    { temperatureUnit : TemperatureUnit
    , measureUnit : MeasureUnit
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
    = Foot
    | Meter


type Msg
    = TemperatureUnit TemperatureUnit
    | MeasureUnit MeasureUnit
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
        ]


resultingPrecipitation : Model -> Html Msg
resultingPrecipitation model =
    let
        ( _, probability ) =
            Frequency.toString (Baseline.precipitationFrequency model)

        intensity : Intensity
        intensity =
            Baseline.precipitationIntensity model

        row : Int -> Int -> String -> String -> Html msg
        row pctMin pctMax label duration =
            Html.tr []
                [ Html.td
                    [ Html.Attributes.style "text-align" "right" ]
                    [ Html.text (String.fromInt pctMin) ]
                , Html.td [] [ Html.text "–" ]
                , Html.td [] [ Html.text (String.fromInt pctMax) ]
                , Html.td [] [ Html.text label ]
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

        data : List (Html msg)
        data =
            case ( intensity, model.currentTemperature /= Below32 ) of
                ( Light, True ) ->
                    [ row 1 20 "Light fog" "1d8"
                    , row 21 40 "Medium fog" "1d6"
                    , row 41 50 "Drizzle" "1d4"
                    , row 51 75 "Drizzle" "2d12"
                    , row 76 90 "Light rain" "1d4"
                    , row 91
                        100
                        (if model.currentTemperature == Above40 then
                            "Light rain"

                         else
                            "Sleet"
                        )
                        "1"
                    ]

                ( Light, False ) ->
                    [ row 1 20 "Light fog" "1d6"
                    , row 21 40 "Light fog" "1d8"
                    , row 41 50 "Medium fog" "1d4"
                    , row 51 60 "Light snow" "1"
                    , row 61 75 "Light snow" "1d4"
                    , row 76 100 "Light snow" "2d12"
                    ]

                ( Medium, True ) ->
                    [ row 1 10 "Medium fog" "1d8"
                    , row 11 20 "Medium fog" "1d12"
                    , row 21 30 "Heavy fog" "1d4"
                    , row 31 35 "Rain" "1d4"
                    , row 36 70 "Rain" "1d8"
                    , row 71 90 "Rain" "2d12"
                    , row 91
                        100
                        (if model.currentTemperature == Above40 then
                            "Rain"

                         else
                            "Sleet"
                        )
                        "1d4"
                    ]

                ( Medium, False ) ->
                    [ row 1 10 "Medium fog" "1d6"
                    , row 11 20 "Medium fog" "1d8"
                    , row 21 30 "Heavy fog" "1d4"
                    , row 31 50 "Medium snow" "1d4"
                    , row 51 90 "Medium snow" "1d8"
                    , row 91 100 "Medium snow" "2d12"
                    ]

                ( Heavy, True ) ->
                    [ row 1 10 "Heavy fog" "1d8"
                    , row 11 20 "Heavy fog" "2d6"
                    , row 21 50 "Heavy rain" "1d12"
                    , row 51 70 "Heavy rain" "2d12"
                    , row 71
                        85
                        (if model.currentTemperature == Above40 then
                            "Heavy rain"

                         else
                            "Sleet"
                        )
                        "1d8"
                    , row 86 90 "Thunderstorm" "1"
                    , row 91 100 "Thunderstorm" "1d3"
                    ]

                ( Heavy, False ) ->
                    [ row 1 10 "Medium fog" "1d8"
                    , row 11 20 "Heavy fog" "2d6"
                    , row 21 60 "Light snow" "2d12"
                    , row 61 90 "Medium snow" "1d8"
                    , row 91 100 "Heavy snow" "1d6"
                    ]

                ( Torrential, True ) ->
                    [ row 1 5 "Heavy fog" "1d8"
                    , row 6 10 "Heavy fog" "2d6"
                    , row 11 30 "Heavy rain" "2d6"
                    , row 31 60 "Heavy rain" "2d12"
                    , row 61
                        80
                        (if model.currentTemperature == Above40 then
                            "Heavy rain"

                         else
                            "Sleet"
                        )
                        "2d6"
                    , row 81 95 "Thunderstorm" "1d3"
                    , row 96 100 "Thunderstorm" "1d6"
                    ]

                ( Torrential, False ) ->
                    [ row 1 5 "Heavy fog" "1d8"
                    , row 6 10 "Heavy fog" "2d6"
                    , row 11 50 "Heavy snow" "1d4"
                    , row 51 90 "Heavy snow" "1d8"
                    , row 91 100 "Heavy snow" "2d12"
                    ]
    in
    boxxxy "Result"
        [ Html.p []
            [ Html.text ("Roll the daily probability (" ++ String.fromInt probability ++ "%).")
            ]
        , Html.p [ Html.Attributes.style "inline-size" "contain" ]
            [ Html.text "If there is precipitation, "
            , Html.br [] []
            , Html.text "roll a random starting hour."
            ]
        , Html.table [] (header :: data)
        ]


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
                Html.tr []
                    [ Html.td
                        [ Html.Attributes.style "text-align" "right" ]
                        [ Html.text (String.fromInt pctMin) ]
                    , Html.td [] [ Html.text "–" ]
                    , Html.td [] [ Html.text (String.fromInt pctMax) ]
                    , Html.td [] [ Html.text variation ]
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
    boxxxy "Variations"
        [ Html.table [] (header :: rows) ]


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
