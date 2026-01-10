module Theme exposing (button, temperatureColor, toggle)

import Color exposing (Color)
import Color.Oklch as Oklch exposing (Oklch)
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
        , color : Maybe Color
        , background : Maybe Color
        }
    -> Html msg
toggle attrs config =
    Html.button
        ([ Html.Attributes.style "color"
            (config.color
                |> Maybe.map Color.toCssString
                |> Maybe.withDefault "inherit"
            )
         , Html.Attributes.style "background"
            (if config.selected then
                config.background
                    |> Maybe.map Color.toCssString
                    |> Maybe.withDefault "transparent"

             else
                let
                    bgColor : String
                    bgColor =
                        let
                            oklch : Oklch
                            oklch =
                                config.background
                                    |> Maybe.withDefault Color.white
                                    |> Oklch.fromColor
                        in
                        { oklch
                            | chroma = oklch.chroma * 0.7
                            , lightness = (oklch.lightness - 0.5) * 0.6 + 0.5
                        }
                            |> Oklch.toCssString

                    stripeColor : String
                    stripeColor =
                        let
                            oklch : Oklch
                            oklch =
                                config.background
                                    |> Maybe.withDefault Color.white
                                    |> Oklch.fromColor
                        in
                        { oklch | lightness = (oklch.lightness - 0.5) * 0.6 + 0.5 }
                            |> Oklch.toCssString
                in
                cssFunction "repeating-linear-gradient"
                    [ "135deg"
                    , bgColor
                    , bgColor ++ " 10px"
                    , stripeColor ++ " 10px"
                    , stripeColor ++ " 11.44px"
                    ]
            )
         , Html.Attributes.style "weight" "bold"
         , Html.Attributes.style "border" "1px solid black"
         , Html.Events.onClick config.onPress
         ]
            ++ attrs
        )
        [ Html.text config.label ]


cssFunction : String -> List String -> String
cssFunction name args =
    name ++ "(" ++ String.join ", " args ++ ")"


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
