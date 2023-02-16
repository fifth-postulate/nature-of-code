module Basis.Coordinate exposing (Axis(..), Coordinate, coordinate, project)


type Coordinate
    = Coordinate ( Float, Float )


coordinate : Float -> Float -> Coordinate
coordinate x y =
    Coordinate ( x, y )


type Axis
    = X
    | Y


project : Axis -> Coordinate -> Float
project axis (Coordinate c) =
    case axis of
        X ->
            Tuple.first c

        Y ->
            Tuple.second c
