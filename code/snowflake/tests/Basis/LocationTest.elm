module Basis.LocationTest exposing (suite)

import Basis.Location as Location exposing (location)
import Expect 
import Fuzz exposing (int, pair)
import Test exposing (Test, describe, fuzz2)


suite : Test
suite =
    describe "Location"
        [ describe "are determined by their components"
            [ fuzz2 int int "first argument to location corresponds with first" <|
                \a b ->
                    let
                        l =
                            location a b
                    in
                    Expect.equal a <| Location.first l
            , fuzz2 int int "second argument to location corresponds with second" <|
                \a b ->
                    let
                        l =
                            location a b
                    in
                    Expect.equal b <| Location.second l
            ]
        , describe "can be added"
            [ let
                pair_of_int =
                    pair int int
              in
              fuzz2 pair_of_int pair_of_int "additions is componentwise" <|
                \( a, b ) ( c, d ) ->
                    let
                        s =
                            location a b

                        t =
                            location c d

                        actual =
                            Location.add s t

                        expected =
                            location (a + c) (b + d)
                    in
                    Expect.equal actual expected
            ]
        ]
