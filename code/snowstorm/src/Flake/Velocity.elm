module Flake.Velocity exposing (Velocity, displacement, velocity, zero)

import Flake.Location exposing (Location, location)
import Flake.Time as Time exposing (Time)


type Velocity
    = Velocity { vx : Float, vy : Float }


zero : Velocity
zero =
    velocity 0 0


velocity : Float -> Float -> Velocity
velocity vx vy =
    Velocity { vx = vx, vy = vy }


displacement : Time -> Velocity -> Location
displacement time (Velocity { vx, vy }) =
    let
        t =
            Time.milliseconds time
    in
    location (vx * t) (vy * t)
