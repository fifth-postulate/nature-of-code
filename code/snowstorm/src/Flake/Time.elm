module Flake.Time exposing (Time, milliseconds, ms)


type Time
    = Milliseconds Float


ms : Float -> Time
ms =
    Milliseconds


milliseconds : Time -> Float
milliseconds (Milliseconds t) =
    t
