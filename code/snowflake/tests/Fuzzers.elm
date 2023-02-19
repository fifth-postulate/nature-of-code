module Fuzzers exposing (coordinate, transform)

import Basis.Coordinate as Coordinate exposing (Coordinate)
import Basis.Transform as Transform exposing (Transform)
import Fuzz exposing (Fuzzer, floatRange)


float: Fuzzer Float
float = floatRange -1e20 1e20

coordinate : Fuzzer Coordinate
coordinate =
    Fuzz.map2 Coordinate.coordinate float float

transform: Fuzzer Transform
transform =
    Fuzz.map6 Transform.transform float float float float float float