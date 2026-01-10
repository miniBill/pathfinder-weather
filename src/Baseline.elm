module Baseline exposing (averageTemperature, precipitationFrequency, precipitationIntensity)

import Altitude exposing (Altitude(..))
import Climate exposing (Climate(..))
import Frequency exposing (Frequency(..))
import Intensity exposing (Intensity(..))
import Quantity
import Season exposing (Season(..))
import Temperature exposing (Temperature)


averageTemperature : { a | climate : Climate, season : Season, altitude : Altitude } -> Temperature
averageTemperature { climate, season, altitude } =
    let
        baseline : Temperature
        baseline =
            case ( climate, season ) of
                ( Cold, Winter ) ->
                    Temperature.degreesFahrenheit 20

                ( Cold, Spring ) ->
                    Temperature.degreesFahrenheit 30

                ( Cold, Summer ) ->
                    Temperature.degreesFahrenheit 40

                ( Cold, Fall ) ->
                    Temperature.degreesFahrenheit 30

                ( Temperate, Winter ) ->
                    Temperature.degreesFahrenheit 30

                ( Temperate, Spring ) ->
                    Temperature.degreesFahrenheit 60

                ( Temperate, Summer ) ->
                    Temperature.degreesFahrenheit 80

                ( Temperate, Fall ) ->
                    Temperature.degreesFahrenheit 60

                ( Tropical, Winter ) ->
                    Temperature.degreesFahrenheit 50

                ( Tropical, Spring ) ->
                    Temperature.degreesFahrenheit 75

                ( Tropical, Summer ) ->
                    Temperature.degreesFahrenheit 95

                ( Tropical, Fall ) ->
                    Temperature.degreesFahrenheit 75

        adjustment : Temperature.Delta
        adjustment =
            case altitude of
                SeaLevel ->
                    Temperature.fahrenheitDegrees 10

                Lowland ->
                    Quantity.zero

                Highland ->
                    Temperature.fahrenheitDegrees -10
    in
    baseline |> Temperature.plus adjustment


precipitationIntensity : { a | altitude : Altitude, climate : Climate } -> Intensity
precipitationIntensity { altitude, climate } =
    let
        baseline : Intensity
        baseline =
            case altitude of
                SeaLevel ->
                    Heavy

                Lowland ->
                    Medium

                Highland ->
                    Medium
    in
    case climate of
        Cold ->
            Intensity.decrease baseline

        Temperate ->
            baseline

        Tropical ->
            Intensity.increase baseline


precipitationFrequency : { a | climate : Climate, season : Season, altitude : Altitude } -> Frequency
precipitationFrequency { climate, season, altitude } =
    let
        baseline : Frequency
        baseline =
            case ( climate, season ) of
                ( Cold, Winter ) ->
                    Drought

                ( Cold, Spring ) ->
                    Rare

                ( Cold, Summer ) ->
                    Intermittent

                ( Cold, Fall ) ->
                    Rare

                ( Temperate, Winter ) ->
                    Rare

                ( Temperate, Spring ) ->
                    Intermittent

                ( Temperate, Summer ) ->
                    Common

                ( Temperate, Fall ) ->
                    Intermittent

                ( Tropical, Winter ) ->
                    Intermittent

                ( Tropical, Spring ) ->
                    Constant

                ( Tropical, Summer ) ->
                    Common

                ( Tropical, Fall ) ->
                    Constant
    in
    case altitude of
        SeaLevel ->
            baseline

        Lowland ->
            baseline

        Highland ->
            Frequency.decrease baseline
