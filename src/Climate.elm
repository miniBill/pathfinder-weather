module Climate exposing (Climate(..), adjustPrecipitation, averageTemperature, toString)

import Season exposing (Season(..))
import Temperature exposing (Temperature)


type Climate
    = Cold
    | Temperate
    | Tropical


toString : Climate -> String
toString name =
    case name of
        Cold ->
            "Cold"

        Temperate ->
            "Temperate"

        Tropical ->
            "Tropical"


adjustPrecipitation : Climate -> Int
adjustPrecipitation name =
    case name of
        Cold ->
            -1

        Temperate ->
            0

        Tropical ->
            1


averageTemperature : Climate -> Season -> Temperature
averageTemperature climate season =
    let
        fahrenheit : Float
        fahrenheit =
            case ( climate, season ) of
                ( Cold, Winter ) ->
                    20

                ( Cold, Summer ) ->
                    40

                ( Cold, _ ) ->
                    30

                ( Temperate, Winter ) ->
                    30

                ( Temperate, Summer ) ->
                    80

                ( Temperate, _ ) ->
                    60

                ( Tropical, Winter ) ->
                    50

                ( Tropical, Summer ) ->
                    95

                ( Tropical, _ ) ->
                    75
    in
    Temperature.degreesFahrenheit fahrenheit
