module Basis.Transform exposing (Transform, image, rotate, scale, transform, translate)

import Basis.Coordinate as Coordinate exposing (Axis(..), Coordinate, project)


type Transform
    = Transform Data


type alias Data =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , tx : Float
    , ty : Float
    }


transform : Float -> Float -> Float -> Float -> Float -> Float -> Transform
transform a b c d tx ty =
    Transform { a = a, b = b, c = c, d = d, tx = tx, ty = ty }


image : Transform -> Coordinate -> Coordinate
image (Transform { a, b, c, d, tx, ty }) z =
    let
        x =
            project X z

        y =
            project Y z

        s =
            a * x + c * y + tx

        t =
            b * y + d * y + ty
    in
    Coordinate.coordinate s t


scale : Float -> Float -> Transform
scale sx sy =
    transform sx 0 0 sy 0 0


rotate : Float -> Transform
rotate angle =
    let
        a =
            cos angle

        b =
            sin angle

        c =
            negate <| sin angle

        d =
            cos angle
    in
    transform a b c d 0 0


translate : Float -> Float -> Transform
translate tx ty =
    transform 0 0 0 0 tx ty
