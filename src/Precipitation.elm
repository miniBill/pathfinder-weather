module Precipitation exposing (Precipitation(..), description, toString)


type Precipitation
    = Drizzle
    | HeavyFog
    | LightFog
    | MediumFog
    | LightRain
    | Rain
    | HeavyRain
    | Sleet
    | HeavySnow
    | LightSnow
    | MediumSnow
    | Thunderstorm


toString : Precipitation -> String
toString precipitation =
    case precipitation of
        Drizzle ->
            "Drizzle"

        HeavyFog ->
            "Heavy fog"

        LightFog ->
            "Light fog"

        MediumFog ->
            "Medium fog"

        LightRain ->
            "Light rain"

        Rain ->
            "Rain"

        HeavyRain ->
            "Heavy rain"

        Sleet ->
            "Sleet"

        HeavySnow ->
            "Heavy snow"

        LightSnow ->
            "Light snow"

        MediumSnow ->
            "Medium snow"

        Thunderstorm ->
            "Thunderstorm"


description : Precipitation -> String
description precipitation =
    case precipitation of
        Drizzle ->
            "Reduces visibility to three-quarters of the normal range, imposing a –2 penalty on Perception checks. It automatically extinguishes tiny unprotected flames (candles and the like, but not torches)."

        HeavyFog ->
            "Heavy fog obscures all vision beyond 5 feet, including darkvision. Creatures 5 feet away have concealment. Heavy fog typically occurs early in the day, late in the day, or sometimes at night, but the heat of the midday usually burns it away. Heavy fog occurs only when there is no or light wind."

        LightFog ->
            "Light fog reduces visibility to three-quarters of the normal ranges, resulting in a –2 penalty on Perception checks and a –2 penalty on ranged attacks. Light fog typically occurs early in the day, late in the day, or sometimes at night, but the heat of the midday usually burns it away. Light fog occurs only when there is no or light wind."

        MediumFog ->
            "Medium fog reduces visibility ranges by half, resulting in a –4 penalty on Perception checks and a –4 penalty on ranged attacks. Medium fog typically occurs early in the day, late in the day, or sometimes at night, but the heat of the midday usually burns it away. Medium fog occurs only when there is no or light wind."

        LightRain ->
            "Rain reduces visibility ranges by half, resulting in a –4 penalty on Perception checks. Rain automatically extinguishes unprotected flames (candles, torches, and the like) and imposes a –4 penalty on ranged attacks."

        Rain ->
            "Rain reduces visibility ranges by half, resulting in a –4 penalty on Perception checks. Rain automatically extinguishes unprotected flames (candles, torches, and the like) and imposes a –4 penalty on ranged attacks."

        HeavyRain ->
            "Heavy rain reduces visibility to one-quarter of the normal range, resulting in a –6 penalty on Perception checks. Heavy rain automatically extinguishes unprotected flames and imposes a –6 penalty on ranged attacks."

        Sleet ->
            "Essentially frozen rain, sleet has the same effect as light snow, but any accumulation typically doesn’t last longer than 1–2 hours after the storm."

        HeavySnow ->
            "Heavy snow reduces visibility ranges to one-quarter of the normal range, resulting in a –6 penalty on Perception checks. It extinguishes unprotected flames and imposes a –6 penalty on ranged attacks. Heavy snow impedes movement even before it begins to stick. Moving into a square during a heavy snowstorm requires 1 extra 5-foot square of movement (this stacks with difficult terrain). Every hour of heavy snow leaves 1d4 inches of snow on the ground. As long as at least 2 inches of snow remain on the ground, the requirement of an extra square of movement to enter a square of snow persists. If at least 1 foot of snow remains on the ground, 2 extra squares of movement are required to enter a snow-filled square instead. A heavy snowstorm has a 10% chance of generating thundersnow and has a 40% chance of becoming a blizzard if the wind speed is severe or stronger."

        LightSnow ->
            "Light snow reduces visibility to three-quarters of the normal range, resulting in a –2 penalty on Perception checks. Light snow has a 75% chance each hour of extinguishing unprotected flames and imposes a –2 penalty on ranged attacks. Light snow does not impede movement unless it continues for 2 or more hours, at which point moving into a square of such snow requires 1 extra 5-foot square of movement (this stacks with difficult terrain). Every 2 hours of light snow leaves 1 inch of snow on the ground. As long as at least 2 inches of snow remain on the ground, the requirement of an extra square of movement to enter a square of snow persists. If at least 1 foot of snow remains on the ground, entering a snow-filled square instead requires 2 extra squares of movement."

        MediumSnow ->
            "Medium snow reduces visibility ranges by half, resulting in a –4 penalty on Perception checks. Medium snow extinguishes unprotected flames and imposes a –4 penalty on ranged attacks. Medium snow does not impede movement unless it continues for 1 hour, at which point moving into a square of such snow requires 1 extra 5-foot square of movement (this stacks with difficult terrain). Every hour of medium snow leaves 1 inch of snow on the ground. As long as at least 2 inches of snow remain on the ground, the requirement of an extra square of movement to enter a square of snow persists. If at least 1 foot of snow remains on the ground, entering a snow-filled square instead requires 2 extra squares of movement."

        Thunderstorm ->
            "Thunderstorms feature powerful winds and heavy rain"
