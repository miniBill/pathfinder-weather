module Baseline exposing (averageTemperature, precipitationFrequency, precipitationIntensity)

import Altitude exposing (Altitude(..))
import Climate exposing (Climate(..), Cold(..))
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
                ( Cold Polar, Winter ) ->
                    Temperature.degreesFahrenheit 0

                ( Cold Polar, Spring ) ->
                    Temperature.degreesFahrenheit 10

                ( Cold Polar, Summer ) ->
                    Temperature.degreesFahrenheit 20

                ( Cold Polar, Fall ) ->
                    Temperature.degreesFahrenheit 10

                ( Cold Arctic, Winter ) ->
                    Temperature.degreesFahrenheit 10

                ( Cold Arctic, Spring ) ->
                    Temperature.degreesFahrenheit 20

                ( Cold Arctic, Summer ) ->
                    Temperature.degreesFahrenheit 30

                ( Cold Arctic, Fall ) ->
                    Temperature.degreesFahrenheit 20

                ( Cold Regular, Winter ) ->
                    Temperature.degreesFahrenheit 20

                ( Cold Regular, Spring ) ->
                    Temperature.degreesFahrenheit 30

                ( Cold Regular, Summer ) ->
                    Temperature.degreesFahrenheit 40

                ( Cold Regular, Fall ) ->
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
        Cold _ ->
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
                ( Cold _, Winter ) ->
                    Drought

                ( Cold _, Spring ) ->
                    Rare

                ( Cold _, Summer ) ->
                    Intermittent

                ( Cold _, Fall ) ->
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
