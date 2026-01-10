module Theme exposing (button, temperatureColor, toggle)

import Color exposing (Color)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Temperature exposing (Temperature)


button : List (Attribute msg) -> { label : String, onPress : msg } -> Html msg
button attrs config =
    Html.button
        ([ Html.Attributes.style "color" "inherit"
         , Html.Attributes.style "background" "inherit"
         , Html.Attributes.style "weight" "bold"
         , Html.Attributes.style "border" "1px solid black"
         , Html.Events.onClick config.onPress
         ]
            ++ attrs
        )
        [ Html.text config.label ]


toggle :
    List (Attribute msg)
    ->
        { selected : Bool
        , label : String
        , onPress : msg
        }
    -> Html msg
toggle attrs config =
    Html.button
        ([ Html.Attributes.style "color" "inherit"
         , Html.Attributes.style "background" "inherit"
         , Html.Attributes.style "weight" "bold"
         , Html.Attributes.style "border"
            (if config.selected then
                "1px dashed black"

             else
                "1px solid black"
            )
         , Html.Events.onClick config.onPress
         ]
            ++ attrs
        )
        [ Html.text config.label ]


{-| Source: <https://en.wikipedia.org/w/index.php?title=Module:Weather_box/colors&action=edit>
-}
temperatureColor : Temperature -> ( Color, Color )
temperatureColor temperature =
    let
        val : Float
        val =
            Temperature.inDegreesCelsius temperature

        red : Float
        red =
            if val < 4.5 then
                rescale val -42.75 4.5

            else
                rescale val 60 41.5

        green : Float
        green =
            if val <= 4.5 then
                rescale val -42.75 4.5

            else
                rescale val 41.5 4.5

        blue : Float
        blue =
            if val < -42.78 then
                rescale val -90 -42.78

            else
                rescale val 23 4.5

        textColor : Color
        textColor =
            if val < -23.3 || val >= 37.8 then
                Color.white

            else
                Color.black
    in
    ( textColor, Color.rgb red green blue )


{-| Source: <https://en.wikipedia.org/w/index.php?title=Module:Weather_box/colors&action=edit>
-}
rescale : Float -> Float -> Float -> Float
rescale value start stop =
    ((value - start) / (stop - start))
        |> clamp 0 1
