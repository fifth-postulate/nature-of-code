module Basis.Location exposing (Location, add, first, location, second, toString)


type Location
    = Location { u : Int, v : Int }


location : Int -> Int -> Location
location u v =
    Location { u = u, v = v }


first : Location -> Int
first (Location { u }) =
    u


second : Location -> Int
second (Location { v }) =
    v


add : Location -> Location -> Location
add p q =
    location (first p + first q) (second p + second q)


toString : Location -> String
toString (Location { u, v }) =
    "(" ++ String.fromInt u ++ "," ++ String.fromInt v ++ ")"
