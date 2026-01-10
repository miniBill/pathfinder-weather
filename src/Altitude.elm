module Altitude exposing (Altitude(..), Highland(..), toString)


type Altitude
    = SeaLevel
    | Lowland
    | Highland Highland


type Highland
    = Arid
    | Regular
    | HighAltitude


toString : Altitude -> String
toString altitude =
    case altitude of
        SeaLevel ->
            "Sea level"

        Lowland ->
            "Lowland"

        Highland Regular ->
            "Highland"

        Highland Arid ->
            "Highland (arid)"

        Highland HighAltitude ->
            "Highland (high altitude)"
