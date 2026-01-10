module Frequency exposing (Frequency(..), decrease, increase)


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
