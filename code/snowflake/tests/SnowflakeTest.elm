module SnowflakeTest exposing (suite)

import Basis.Location as Location exposing (Location, location)
import Expect exposing (Expectation, fail, pass)
import Snowflake exposing (Flake)
import Test exposing (Test, describe, test)


snowflake : Snowflake.Flake
snowflake =
    List.singleton <| location 0 0


suite : Test
suite =
    describe "snowflake"
        [ describe "isNear (0,0)"
            ([ location 1 0, location 0 1, location -1 1, location -1 0, location 0 -1, location 1 -1 ]
                |> List.map (isNearToTest snowflake)
            )
        ]


isNearToTest : Flake -> Location -> Test
isNearToTest s l =
    test (Location.toString l) <|
        \_ ->
            isTrue <| Snowflake.isNear s l


isTrue : Bool -> Expectation
isTrue b =
    if b then
        pass

    else
        fail "expected flag to be true"
