module Climate exposing (Climate(..), toString)


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
