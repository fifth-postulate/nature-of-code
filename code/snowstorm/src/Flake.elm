module Flake exposing (Model, at, update, view)

import Debug
import Flake.Location as Location exposing (Location, location)
import Flake.Time exposing (Time)
import Flake.Velocity as Velocity exposing (Velocity, displacement)
import Html exposing (Html)


type Model
    = Flake
        { location : Location
        , velocity : Velocity
        }


at : Location -> Model
at location =
    Flake { location = location, velocity = Velocity.zero }


update : Time -> Model -> Model
update time (Flake flake) =
    let
        ds =
            displacement time flake.velocity

        location =
            Location.add flake.location ds
    in
    Flake { flake | location = location }


view : Model -> Html msg
view model =
    Html.text <| Debug.toString model
