module Prime exposing (main)

import Browser
import Deque exposing (Deque)
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Event
import Prime.Stream as Stream exposing (Stream)
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }


init : a -> ( Model, Cmd Msg )
init _ =
    let
        target = 
            300
        configuration =
            { delay = 200 }
    in
    ( { stream = Stream.stream
      , primes = Deque.empty
      , target = target
      , configuration = configuration
      }
    , Cmd.none
    )


type alias Model =
    { stream : Stream
    , primes : Deque Int
    , target : Int
    , configuration : Configuration
    }


type alias Configuration =
    { delay : Float }


type Msg
    = Advance
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                cmd =
                    if Deque.length model.primes < model.target then
                        Task.perform (\_ -> Advance) <| Task.succeed ()

                    else
                        Cmd.none
            in
            ( model, cmd )

        Advance ->
            let
                ( p, stream ) =
                    Stream.advance model.stream
            in
            ( { model | stream = stream, primes = Deque.pushBack p model.primes }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        toCode n =
            Html.code []
                [ Html.text <| String.fromInt n ]

        comma =
            Html.text ", "
    in
    Html.div []
        [ Html.button [ Event.onClick Advance ] [ Html.text "𝌙" ]
        , Html.blockquote [] <| List.intersperse comma <| Deque.toList <| Deque.map toCode model.primes
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every model.configuration.delay (\_ -> Tick)
        ]
