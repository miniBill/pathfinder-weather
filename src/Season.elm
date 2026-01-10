module Season exposing (Season(..), toString)


type Season
    = Winter
    | Spring
    | Summer
    | Fall


toString : Season -> String
toString season =
    case season of
        Winter ->
            "Winter"

        Spring ->
            "Spring"

        Summer ->
            "Summer"

        Fall ->
            "Fall"
