module Intensity exposing (Intensity(..), decrease, increase, toString)


type Intensity
    = Light
    | Medium
    | Heavy
    | Torrential


increase : Intensity -> Intensity
increase intensity =
    case intensity of
        Light ->
            Medium

        Medium ->
            Heavy

        Heavy ->
            Torrential

        Torrential ->
            Torrential


decrease : Intensity -> Intensity
decrease intensity =
    case intensity of
        Light ->
            Light

        Medium ->
            Light

        Heavy ->
            Medium

        Torrential ->
            Heavy


toString : Intensity -> String
toString intensity =
    case intensity of
        Light ->
            "Light"

        Medium ->
            "Medium"

        Heavy ->
            "Heavy"

        Torrential ->
            "Torrential"
