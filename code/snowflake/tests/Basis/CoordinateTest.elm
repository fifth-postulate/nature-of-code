module Basis.CoordinateTest exposing (suite)

import Basis.Coordinate as Coordinate exposing (Axis(..), coordinate)
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (niceFloat)
import Test exposing (Test, describe, fuzz2)


epsilon : FloatingPointTolerance
epsilon =
    Absolute 0.000000001


suite : Test
suite =
    describe "Coordinate"
        [ describe "are determined by their components"
            [ fuzz2 niceFloat niceFloat "first argument to coordinate corresponds with project X" <|
                \a b ->
                    let
                        c =
                            coordinate a b
                    in
                    Expect.within epsilon a <| Coordinate.project X c
            , fuzz2 niceFloat niceFloat "second argument to coordinate corresponds with project Y" <|
                \a b ->
                    let
                        c =
                            coordinate a b
                    in
                    Expect.within epsilon b <| Coordinate.project Y c
            ]
        ]
