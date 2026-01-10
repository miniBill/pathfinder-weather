module Frequency exposing (Frequency(..), decrease, increase, toString)


type Frequency
    = Drought
    | Rare
    | Intermittent
    | Common
    | Constant


increase : Frequency -> Frequency
increase frequency =
    case frequency of
        Drought ->
            Rare

        Rare ->
            Intermittent

        Intermittent ->
            Common

        Common ->
            Constant

        Constant ->
            Constant


decrease : Frequency -> Frequency
decrease frequency =
    case frequency of
        Drought ->
            Drought

        Rare ->
            Drought

        Intermittent ->
            Rare

        Common ->
            Intermittent

        Constant ->
            Common


toString : Frequency -> String
toString frequency =
    case frequency of
        Drought ->
            "Drought - 5%"

        Rare ->
            "Rare - 15%"

        Intermittent ->
            "Intermittent - 30%"

        Common ->
            "Common - 60%"

        Constant ->
            "Constant - 95%"
