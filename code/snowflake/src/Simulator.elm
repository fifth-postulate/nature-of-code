port module Simulator exposing (..)

import Basis
import Basis.Location as Location exposing (Location, location)
import Basis.Transform exposing (transform)
import Browser
import Css
import File.Download as Download
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Random exposing (generate)
import Snowflake exposing (Flake, ViewBox)
import Task
import Time exposing (every)


port requestSvg : String -> Cmd msg


port receiveSvg : (String -> msg) -> Sub msg


type alias Model =
    { configuration : Configuration
    , seed : Flake
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
            , minimumNumberOfParticles = 7
            , maximumNumberOfParticles = 500
            }

        seed =
            [ location 0 0
            , location 1 0
            , location 0 1
            , location -1 1
            , location -1 0
            , location 0 -1
            , location 1 -1
            ]

        scene =
            Snowflake.scene seed
    in
    Browser.element
        { init = init configuration seed
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }


viewBoxFromWidth : Int -> ViewBox
viewBoxFromWidth width =
    { width = width, minimum = negate <| width // 2 }


init : Configuration -> Flake -> a -> ( Model, Cmd Msg )
init configuration seed _ =
    let
        snowflake =
            Snowflake.scene seed
    in
    ( { configuration = configuration
      , seed = seed
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
    Html.div [ Attribute.css [ Css.displayFlex, Css.alignItems Css.center ] ]
        [ Html.button [ Event.onClick Reset ] [ Html.text "↻" ]
        , Html.input
            [ Attribute.type_ "range"
            , Attribute.min <| String.fromInt configuration.minimumNumberOfParticles
            , Attribute.max <| String.fromInt configuration.maximumNumberOfParticles
            , Attribute.step <| "6"
            , Attribute.value <| String.fromInt numberOfParticles
            , Event.onInput ChangeNumberOfParticles
            ]
            []
        , Html.span [] [ Html.text <| String.fromInt <| numberOfParticles ]
        , Html.span [ Attribute.css [ Css.marginLeft <| Css.px 5 ] ] [ Html.text <| String.fromInt <| Snowflake.size snowflake ]
        , Html.button [ Event.onClick Download ] [ Html.text "⭳" ]
        ]


type Msg
    = Tick
    | Spawn Location
    | Move Location
    | ChangeNumberOfParticles String
    | Reset
    | Download
    | ReceivedSvg String
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

        Reset ->
            ( { model | snowflake = Snowflake.scene model.seed }, Cmd.none )

        Download ->
            ( model, requestSvg "please" )

        ReceivedSvg source ->
            ( model, Download.string "snowflake.svg" "image/svg+xml" source )

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
        , receiveSvg ReceivedSvg
        ]
