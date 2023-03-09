module Storm exposing (main)

import Browser
import Flake
import Flake.Location exposing (location)
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : a -> ( Model, Cmd Msg )
init _ =
    let
        flake =
            Flake.at <| location 0 0
    in
    ( { flakes = List.singleton flake }, Cmd.none )


type alias Model =
    { flakes : List Flake.Model }


type Msg
    = Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [] <| List.map Flake.view model.flakes


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
