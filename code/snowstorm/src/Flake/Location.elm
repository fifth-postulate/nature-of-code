module Flake.Location exposing (Location, add, location)


type Location
    = Location { x : Float, y : Float }


location : Float -> Float -> Location
location x y =
    Location { x = x, y = y }


add : Location -> Location -> Location
add (Location left) (Location right) =
    Location { x = left.x + right.x, y = left.y + right.y }
