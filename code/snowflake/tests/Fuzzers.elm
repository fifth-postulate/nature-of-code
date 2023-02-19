module Fuzzers exposing (coordinate, transform)

import Basis.Coordinate as Coordinate exposing (Coordinate)
import Basis.Location as Location exposing (Location)
import Basis.Transform as Transform exposing (Transform)
import Fuzz exposing (Fuzzer, floatRange, int)


float : Fuzzer Float
float =
    floatRange -1.0e20 1.0e20


location : Fuzzer Location
location =
    Fuzz.map2 Location.location int int


coordinate : Fuzzer Coordinate
coordinate =
    Fuzz.map2 Coordinate.coordinate float float


transform : Fuzzer Transform
transform =
    Fuzz.map6 Transform.transform float float float float float float
