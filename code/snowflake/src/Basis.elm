module Basis exposing (Basis, basis, coordinate, standard, transform)

import Basis.Coordinate as Coordinate exposing (Axis(..), Coordinate, project)
import Basis.Location as Location exposing (Location)
import Basis.Transform as Transform exposing (Transform)


type Basis
    = Basis
        { e1 : Coordinate
        , e2 : Coordinate
        }


basis : Coordinate -> Coordinate -> Basis
basis e1 e2 =
    Basis { e1 = e1, e2 = e2 }


transform : Transform -> Basis -> Basis
transform t (Basis { e1, e2 }) =
    Basis { e1 = Transform.image t e1, e2 = Transform.image t e2 }


standard : Basis
standard =
    Basis
        { e1 = Coordinate.coordinate 1 0
        , e2 = Coordinate.coordinate 0 1
        }


coordinate : Basis -> Location -> Coordinate
coordinate (Basis { e1, e2 }) location =
    let
        x1 =
            project X e1

        y1 =
            project Y e1

        x2 =
            project X e2

        y2 =
            project Y e2

        s =
            toFloat <| Location.first location

        t =
            toFloat <| Location.second location

        x =
            x1 * s + x2 * t

        y =
            y1 * s + y2 * t
    in
    Coordinate.coordinate x y
