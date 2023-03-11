module Storm exposing (main)

import Browser
import Flake
import Flake.Location exposing (location)
import Svg exposing (Svg)
import Svg.Attributes as Attribute


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
        flakes =
            List.range 0 20
                |> List.map ((*) 20)
                |> List.map toFloat
                |> List.map (\x -> Flake.at <| location x 0)
    in
    ( { flakes = flakes }, Cmd.none )


type alias Model =
    { flakes : List Flake.Model }


type Msg
    = Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Svg Msg
view model =
    Svg.svg
        [ Attribute.id "snowstorm"
        , Attribute.width "640"
        , Attribute.height "480"
        ]
        [ Svg.circle [ Attribute.id "flake", Attribute.cx "0", Attribute.cy "0", Attribute.r "4" ] []
        , Svg.g [] <| List.map Flake.view model.flakes
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
