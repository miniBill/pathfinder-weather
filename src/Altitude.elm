module Altitude exposing (Altitude(..), toString)


type Altitude
    = SeaLevel
    | Lowland
    | Highland


toString : Altitude -> String
toString altitude =
    case altitude of
        SeaLevel ->
            "Sea level"

        Lowland ->
            "Lowland"

        Highland ->
            "Highland"
