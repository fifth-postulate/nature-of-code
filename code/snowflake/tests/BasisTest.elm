module BasisTest exposing (suite)

import Basis
import Basis.Coordinate as Coordinate
import Basis.Location as Location
import Basis.Transform as Transform
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (int)
import Fuzzers exposing (coordinate)
import Test exposing (Test, describe, fuzz2, fuzz3)


epsilon : FloatingPointTolerance
epsilon =
    Absolute 0.000000001


suite : Test
suite =
    describe "Basis"
        [ describe "transform"
            [ fuzz3 coordinate coordinate Fuzzers.transform "as the coordinates images" <|
                \a b t ->
                    let
                        original =
                            Basis.basis a b

                        actual =
                            Basis.transform t original

                        expected =
                            Basis.basis
                                (Transform.image t a)
                                (Transform.image t b)
                    in
                    Expect.equal actual expected
            ]
        , describe "standardbase"
            [ fuzz2 int int "treats locations as coordinates" <|
                \a b ->
                    let
                        l =
                            Location.location a b

                        actual =
                            Basis.coordinate Basis.standard l

                        expected =
                            Coordinate.coordinate (toFloat a) (toFloat b)
                    in
                    Expect.equal actual expected
            ]
        ]
