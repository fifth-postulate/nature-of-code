module Snowflake exposing (Configuration, Flake, Model, Msg(..), Path, ViewBox, hasPath, isNear, pathGuard, scene, size, update, view)

import Basis exposing (Basis, coordinate)
import Basis.Coordinate exposing (Axis(..), project)
import Basis.Location as Location exposing (Location, location)
import Svg exposing (Svg, a)
import Svg.Attributes as Attribute


type alias Configuration =
    { size : Int
    , viewBox : ViewBox
    , radius : Int
    , basis : Basis
    }


type alias ViewBox =
    { width : Int
    , minimum : Int
    }


type Model
    = Scene
        { flake : Flake
        , path : Path
        }


hasPath : Model -> Bool
hasPath (Scene { path }) =
    not <| List.isEmpty path


size : Model -> Int
size (Scene { flake }) =
    List.length flake


pathGuard : (Location -> Bool) -> Model -> Bool
pathGuard predicate (Scene { path }) =
    path
        |> List.head
        |> Maybe.map predicate
        |> Maybe.withDefault False


type alias Flake =
    List Location


type alias Path =
    List Location


scene : Flake -> Model
scene seed =
    Scene
        { flake = seed
        , path = []
        }


type Msg
    = Spawn Location
    | Move Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Scene s) =
    case msg of
        Spawn location ->
            ( Scene { s | path = [ location ] }, Cmd.none )

        Move delta ->
            let
                h =
                    s.path
                        |> List.head
                        |> Maybe.map (Location.add delta)

                nearFlake =
                    h
                        |> Maybe.map (isNear s.flake)
                        |> Maybe.withDefault False
            in
            if nearFlake then
                let
                    head =
                        h
                            |> Maybe.map symmetries
                            |> Maybe.withDefault []
                in
                ( Scene { s | path = [], flake = List.append head s.flake }, Cmd.none )

            else
                let
                    head =
                        h
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                in
                ( Scene { s | path = List.append head s.path }, Cmd.none )


symmetries : Location -> List Location
symmetries l =
    let
        a =
            Location.first l

        b =
            Location.second l
    in
    [ location a b
    , location -b (a + b)
    , location (-a - b) a
    , location -a -b
    , location b (-a - b)
    , location (a + b) -a
    ]


isNear : Flake -> Location -> Bool
isNear flake l =
    let
        neighbourhood =
            [ location 1 0, location 0 1, location -1 1, location -1 0, location 0 -1, location 1 -1 ]
                |> List.map (Location.add l)
    in
    neighbourhood
        |> List.map (swap List.member flake)
        |> List.any identity


swap : (a -> b -> c) -> b -> a -> c
swap f b a =
    f a b


view : Configuration -> Model -> Svg a
view configuration (Scene s) =
    Svg.svg
        [ Attribute.width <| String.fromInt configuration.size
        , Attribute.height <| String.fromInt configuration.size
        , Attribute.viewBox <| viewBoxDescription configuration.viewBox
        ]
        [ viewPath configuration s.path
        , viewFlake configuration s.flake
        ]


viewBoxDescription : ViewBox -> String
viewBoxDescription { width, minimum } =
    [ minimum, minimum, width, width ]
        |> List.map String.fromInt
        |> String.join " "


viewPath : Configuration -> Path -> Svg a
viewPath configuration path =
    let
        particle =
            path
                |> List.head
                |> Maybe.map (viewParticle configuration)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
                |> Svg.g []
    in
    Svg.g [ Attribute.fill "red" ]
        [ viewTrace configuration path
        , particle
        ]


viewTrace : { e | basis : Basis } -> Path -> Svg a
viewTrace { basis } path =
    let
        toPoint l =
            let
                c =
                    coordinate basis l

                x =
                    project X c

                y =
                    project Y c
            in
            [ x, y ]
                |> List.map String.fromFloat
                |> String.join ","

        points =
            path
                |> List.map toPoint
                |> String.join " "
    in
    Svg.polyline [ Attribute.fill "none", Attribute.stroke "gray", Attribute.points points ] []


viewFlake : Configuration -> Flake -> Svg a
viewFlake configuration flake =
    Svg.g [] <| List.map (viewParticle configuration) flake


viewParticle : { e | radius : Int, basis : Basis } -> Location -> Svg a
viewParticle { radius, basis } l =
    let
        c =
            coordinate basis l

        x =
            project X c

        y =
            project Y c
    in
    Svg.circle [ Attribute.cx <| String.fromFloat x, Attribute.cy <| String.fromFloat y, Attribute.r <| String.fromInt radius ] []
