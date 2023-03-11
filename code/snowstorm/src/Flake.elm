module Flake exposing (Model, at, update, view)

import Flake.Location as Location exposing (Location, location)
import Flake.Time exposing (Time)
import Flake.Velocity as Velocity exposing (Velocity, displacement)
import Svg exposing (Svg)
import Svg.Attributes as Attribute


type Model
    = Flake
        { id : String
        , location : Location
        , velocity : Velocity
        }


at : Location -> Model
at location =
    Flake { id = "flake", location = location, velocity = Velocity.zero }


update : Time -> Model -> Model
update time (Flake flake) =
    let
        ds =
            displacement time flake.velocity

        location =
            Location.add flake.location ds
    in
    Flake { flake | location = location }


view : Model -> Svg msg
view (Flake model) =
    let
        fragment identifier =
            "#" ++ identifier
    in
    Svg.use
        [ Attribute.xlinkHref <| fragment model.id
        , Attribute.x <| String.fromFloat <| Location.coordinateX model.location
        , Attribute.y <| String.fromFloat <| Location.coordinateY model.location
        ]
        []
