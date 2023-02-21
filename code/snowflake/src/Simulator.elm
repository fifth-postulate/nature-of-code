module Simulator exposing (..)

import Basis
import Basis.Location as Location exposing (Location, location)
import Basis.Transform exposing (transform)
import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Random exposing (generate)
import Snowflake exposing (ViewBox)
import Svg exposing (Svg)
import Task
import Time exposing (every)


type alias Model =
    { configuration : Configuration
    , snowflake : Snowflake.Model
    , numberOfParticles : Int
    }


type alias Configuration =
    { snowflake : Snowflake.Configuration
    , minimumNumberOfParticles : Int
    , maximumNumberOfParticles : Int
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
            { snowflake =
                { size = 640
                , viewBox = viewBoxFromWidth 1000
                , radius = radius
                , basis = Basis.transform t Basis.standard
                }
            , minimumNumberOfParticles = 10
            , maximumNumberOfParticles = 500
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
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }


viewBoxFromWidth : Int -> ViewBox
viewBoxFromWidth width =
    { width = width, minimum = negate <| width // 2 }


init : Configuration -> Snowflake.Model -> a -> ( Model, Cmd Msg )
init configuration snowflake _ =
    ( { configuration = configuration
      , snowflake = snowflake
      , numberOfParticles = (configuration.maximumNumberOfParticles + configuration.minimumNumberOfParticles) // 2
      }
    , Cmd.none
    )


view : Model -> Html Msg
view ({ configuration, snowflake } as model) =
    Html.div []
        [ viewControls model
        , Snowflake.view configuration.snowflake snowflake
        ]


viewControls : Model -> Html Msg
viewControls { configuration, numberOfParticles, snowflake } =
    Html.div []
        [ Html.input
            [ Attribute.type_ "range"
            , Attribute.min <| String.fromInt configuration.minimumNumberOfParticles
            , Attribute.max <| String.fromInt configuration.maximumNumberOfParticles
            , Attribute.value <| String.fromInt numberOfParticles
            , Event.onInput ChangeNumberOfParticles
            ]
            []
        , Html.span [] [ Html.text <| String.fromInt <| numberOfParticles ]
        , Html.span [] [ Html.text <| String.fromInt <| Snowflake.size snowflake ]
        ]


type Msg
    = Tick
    | Spawn Location
    | Move Location
    | ChangeNumberOfParticles String
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

            else if Snowflake.size model.snowflake < model.numberOfParticles then
                ( model, Task.perform Spawn <| Task.succeed <| location -20 0 )

            else
                ( model, Cmd.none )

        Spawn l ->
            ( model, Task.perform SnowflakeMsg <| Task.succeed <| Snowflake.Spawn l )

        Move l ->
            ( model, Task.perform SnowflakeMsg <| Task.succeed <| Snowflake.Move l )

        ChangeNumberOfParticles input ->
            let
                n =
                    input
                        |> String.toInt
                        |> Maybe.withDefault model.numberOfParticles
            in
            ( { model | numberOfParticles = n }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ every 100 (\_ -> Tick)
        ]
