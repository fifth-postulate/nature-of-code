module Simulator exposing (..)

import Basis
import Basis.Location as Location exposing (Location, location)
import Basis.Transform exposing (transform)
import Browser
import Random exposing (generate)
import Snowflake exposing (ViewBox)
import Svg exposing (Svg)
import Task
import Time exposing (every)


type alias Model =
    { configuration : Snowflake.Configuration
    , snowflake : Snowflake.Model
    }


main : Program () Model Msg
main =
    let
        configuration =
            let
                radius =
                    10

                t =
                    transform (2 * radius) 0 radius (radius * sqrt 3) 0 0
            in
            { size = 640
            , viewBox = viewBoxFromWidth 1000
            , radius = radius
            , basis = Basis.transform t Basis.standard
            }

        scene =
            Snowflake.scene
                [ location 0 0
                , location 1 0
                , location 0 1
                , location -1 1
                , location -1 0
                , location 0 -1
                , location 1 -1
                ]
    in
    Browser.element
        { init = init configuration scene
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


viewBoxFromWidth : Int -> ViewBox
viewBoxFromWidth width =
    { width = width, minimum = negate <| width // 2 }


init : Snowflake.Configuration -> Snowflake.Model -> a -> ( Model, Cmd Msg )
init configuration snowflake flags =
    ( { configuration = configuration, snowflake = snowflake }, Cmd.none )


view : Model -> Svg a
view { configuration, snowflake } =
    Snowflake.view configuration snowflake


type Msg
    = Tick
    | Spawn Location
    | Move Location
    | SnowflakeMsg Snowflake.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SnowflakeMsg msg ->
            let
                ( snowflake, cmd ) =
                    Snowflake.update msg model.snowflake
            in
            ( { model | snowflake = snowflake }, Cmd.map SnowflakeMsg cmd )

        Tick ->
            if Snowflake.hasPath model.snowflake && Snowflake.pathGuard (\p -> 30 > Location.first p) model.snowflake then
                ( model, generate Move <| Random.uniform (location 2 0) [ location 1 1, location 0 1, location 0 0, location 1 -1, location 2 -1 ] )

            else
                ( model, Task.perform Spawn <| Task.succeed <| location -20 0 )

        Spawn l ->
            ( model, Task.perform SnowflakeMsg <| Task.succeed <| Snowflake.Spawn l )

        Move l ->
            ( model, Task.perform SnowflakeMsg <| Task.succeed <| Snowflake.Move l )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ every 100 (\_ -> Tick)
        ]
