module Climate exposing (Climate(..), Cold(..), toString)


type Climate
    = Cold Cold
    | Temperate
    | Tropical


type Cold
    = Regular
    | Arctic
    | Polar


toString : Climate -> String
toString name =
    case name of
        Cold Polar ->
            "Polar"

        Cold Arctic ->
            "Arctic"

        Cold Regular ->
            "Cold"

        Temperate ->
            "Temperate"

        Tropical ->
            "Tropical"
