module Main exposing (..)

import AnimationFrame
import Color
import Dict
import Html exposing (Html, program, div)
import Html.Attributes as Attributes
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Game.Resources as Resources exposing (Resources)
import Keyboard.Extra
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Physics exposing (..)
import Task
import Window


-- PROGRAM


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MSG


type Msg
    = Tick Float
    | Resources Resources.Msg
    | ScreenSize Window.Size
    | Keys Keyboard.Extra.Msg



-- MODEL


type alias Model =
    { resources : Resources
    , time : Float
    , screen : ( Int, Int )
    , camera : Camera
    , keys : List Keyboard.Extra.Key
    , planets : List Planet
    , rocket : Rocket
    }


type alias Planet =
    { color : Color.Color
    , au : Float
    , diameter : Int
    , gravity : Float
    , mass : Float
    }


getPlanetPosition planet =
    let
        scaled =
            scaleEarth planet.diameter

        half =
            scaled / 2
    in
        vec2 (scaleAU planet.au) 0


type alias Rocket =
    Physical
        { planet : Int
        , fuel : Float
        , enginePower : Float
        }


init : ( Model, Cmd Msg )
init =
    ( { resources = Resources.init
      , time = 0
      , screen = ( 800, 600 )
      , camera = Camera.fixedWidth 8 ( scaleAU 1, 0 )
      , keys = []
      , planets =
            [ -- Mercury
              Planet Color.red 0.39 4879 1 1
              -- Venus
            , Planet Color.lightBlue 0.723 12104 1 1
              -- Earth
            , Planet Color.blue 1 12750 1 1
              -- Mars
            , Planet Color.red 1.524 6779 1 1
              -- Jupiter
            , Planet Color.orange 5.2 139822 1 1
              -- Saturn
            , Planet Color.lightYellow 9.6 116464 1 1
              -- Uranus
            , Planet Color.lightBlue 19.2 50724 1 1
              -- Neptune
            , Planet Color.darkBlue 30.1 49244 1 1
            ]
      , rocket =
            let
                position =
                    vec2 (scaleAU 1) (earthSize / 2)
            in
                { planet = 2
                , fuel = 100
                , enginePower = 0
                , position = position
                , rotation = degrees 0
                , velocity = vec2 0 0
                , acceleration = vec2 0 0
                , mass =
                    -- Saturn V in kg
                    41000
                , forces = Dict.fromList []
                }
      }
    , Cmd.batch
        [ Cmd.map Resources
            (Resources.loadTextures
                [ "assets/textures/foo.png" ]
            )
        , getScreenSize
        ]
    )


isColliding : Rocket -> Planet -> Bool
isColliding rocket planet =
    if Vector2.distance (getPlanetPosition planet) rocket.position <= scaleEarth (planet.diameter // 2) then
        True
    else
        False


scale =
    2


scaleEarth km =
    ((toFloat km) / 12750) * scale


earthSize =
    scaleEarth 12750


scaleAU units =
    -- scale * units * 149597871 -- real
    -- scale * units * 1
    -- scale * units * 300
    -- 4 + scale * units * 20
    4 + scale * units * 40


at : List a -> Int -> Maybe a
at xs i =
    if (List.length xs) >= (i + 1) then
        List.take (i + 1) xs
            |> List.reverse
            |> List.head
    else
        Nothing



-- VIEW


view : Model -> Html Msg
view ({ time, screen, camera } as model) =
    div
        [ Attributes.style
            [ ( "overflow", "hidden" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        [ Game.render
            { camera = camera
            , time = time
            , size = screen
            }
            (render model)
        ]


render : Model -> List Renderable
render { resources, screen, planets, rocket } =
    let
        ( width, height ) =
            screen

        rocketPlanet =
            at planets rocket.planet
    in
        List.concat
            [ [ renderSpace width height ]
            , List.map renderPlanet planets
            , [ renderRocket rocket ]
            , renderDebugs rocket planets
            ]


renderRocket : Rocket -> Renderable
renderRocket rocket =
    let
        ( x, y ) =
            Vector2.toTuple rocket.position
    in
        Render.shapeWithOptions Render.rectangle
            { color = Color.white
            , position = ( x, y, 0 )
            , size = ( 0.05, 0.1 )
            , rotation = rocket.rotation
            , pivot = ( 0.5, 0.5 )
            }


renderSpace : Int -> Int -> Renderable
renderSpace width height =
    let
        fWidth =
            toFloat width

        fHeight =
            toFloat height
    in
        Render.shape Render.rectangle
            { color = Color.black
            , position = ( fWidth / -2, fHeight / -2 )
            , size = ( fWidth, fHeight )
            }


sun : Renderable
sun =
    let
        size =
            1391400

        scaled =
            scaleEarth size

        half =
            scaled / 2
    in
        Render.shape Render.circle
            { color = Color.yellow
            , position = ( 0 - half, 0 - half )
            , size = ( scaled, scaled )
            }


renderPlanet : Planet -> Renderable
renderPlanet planet =
    let
        scaled =
            scaleEarth planet.diameter

        ( x, y ) =
            Vector2.toTuple (getPlanetPosition planet)
    in
        Render.shapeWithOptions Render.circle
            { color = planet.color
            , position = ( x, y, 0 )
            , size = ( scaled, scaled )
            , rotation = 0
            , pivot = ( 0.5, 0.5 )
            }



-- sprite resources =
--     Render.sprite
--         { texture = Resources.getTexture "assets/textures/foo.png" resources
--         , position = ( 0, 0 )
--         , size = ( 1, 1 )
--         }
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resources msg ->
            ( { model | resources = Resources.update msg model.resources }, Cmd.none )

        ScreenSize { width, height } ->
            ( { model | screen = ( width, height ) }, Cmd.none )

        Tick dt ->
            let
                { x } =
                    Keyboard.Extra.arrows model.keys
            in
                ( { model
                    | time = dt + model.time
                    , camera = Camera.moveBy ( (toFloat x) / 30, 0 ) model.camera
                    , rocket = tickRocket dt model.keys (at model.planets model.rocket.planet) model.rocket
                  }
                , Cmd.none
                )

        Keys keyMsg ->
            let
                keys =
                    Keyboard.Extra.update keyMsg model.keys
            in
                ( { model | keys = (Debug.log "msg" keys) }, Cmd.none )


tickRocket : Float -> List Keyboard.Extra.Key -> Maybe Planet -> Rocket -> Rocket
tickRocket dt keys maybePlanet rocket =
    rocket
        |> engines keys
        |> gravity maybePlanet
        |> surface maybePlanet
        |> rotation dt keys
        |> Physics.update dt


engines : List Keyboard.Extra.Key -> Rocket -> Rocket
engines keys rocket =
    if List.any ((==) Keyboard.Extra.Space) keys then
        { rocket | enginePower = 1 }
            |> Physics.setForce "enginePower" (Physics.toHeading rocket.rotation |> Vector2.scale 2)
    else
        { rocket | enginePower = 0 }
            |> Physics.setForce "enginePower" (vec2 0 0)


gravity : Maybe Planet -> Rocket -> Rocket
gravity maybePlanet rocket =
    case maybePlanet of
        Just planet ->
            rocket |> Physics.setForce "gravity" (Physics.attract (getPlanetPosition planet) rocket.position)

        Nothing ->
            rocket |> Physics.removeForce "gravity"


surface : Maybe Planet -> Rocket -> Rocket
surface maybePlanet rocket =
    case maybePlanet of
        Just planet ->
            if isColliding rocket planet then
                rocket
                    |> Physics.setForce "surface" (Physics.repulse (getPlanetPosition planet) rocket.position)
                -- |> Physics.removeVelocity
            else
                rocket |> Physics.removeForce "surface"

        Nothing ->
            rocket |> Physics.removeForce "surface"


rotation : Float -> List Keyboard.Extra.Key -> Rocket -> Rocket
rotation dt keys rocket =
    let
        { x } =
            Keyboard.Extra.wasd keys
    in
        { rocket | rotation = rocket.rotation + (toFloat x) * -0.01 }



-- TASKS


getScreenSize : Cmd Msg
getScreenSize =
    Task.perform ScreenSize (Window.size)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs ((\dt -> dt / 1000) >> Tick)
        , Sub.map Keys Keyboard.Extra.subscriptions
        ]



-- DEBUGS


renderDebugs : Rocket -> List Planet -> List Renderable
renderDebugs rocket planets =
    List.concat
        [ [ renderDebugPlanet (at planets rocket.planet) ]
        , renderDebugVelocity rocket
        , renderDebugAcceleration rocket
        , renderDebugForces rocket
        ]


renderDebugPlanet : Maybe Planet -> Renderable
renderDebugPlanet maybePlanet =
    Render.shapeWithOptions Render.ring <|
        case maybePlanet of
            Just planet ->
                let
                    ( x, y ) =
                        Vector2.toTuple (getPlanetPosition planet)
                in
                    { color = Color.white
                    , position = ( x, y, 0 )
                    , size =
                        ( (scaleEarth planet.diameter) * 1.1
                        , (scaleEarth planet.diameter) * 1.1
                        )
                    , rotation = 0
                    , pivot = ( 0.5, 0.5 )
                    }

            Nothing ->
                { color = Color.black
                , position = ( 0, 0, 0 )
                , size = ( 0, 0 )
                , rotation = 0
                , pivot = ( 0.5, 0.5 )
                }


renderDebugVelocity : Rocket -> List Renderable
renderDebugVelocity { position, velocity } =
    [ renderDebugLine (Color.rgb 255 0 128) position 0 ((Vector2.getX velocity) * 100)
    , renderDebugLine (Color.rgb 255 0 128) position (degrees 90) ((Vector2.getY velocity) * 100)
    ]


renderDebugAcceleration : Rocket -> List Renderable
renderDebugAcceleration { position, acceleration } =
    [ renderDebugLine (Color.rgb 0 255 128) position 0 ((Vector2.getX acceleration) * 10000)
    , renderDebugLine (Color.rgb 0 255 128) position (degrees 90) ((Vector2.getY acceleration) * 10000)
    ]


renderDebugForces : Rocket -> List Renderable
renderDebugForces { position, forces } =
    let
        renderDebugForce ( name, force ) =
            renderDebugLine (Color.rgb 0 128 255) position (Physics.toRotation force) (Vector2.distance (vec2 0 0) force)
    in
        forces
            |> Dict.toList
            |> List.map renderDebugForce


renderDebugLine : Color.Color -> Vec2 -> Float -> Float -> Renderable
renderDebugLine color position rotation size =
    let
        ( x, y ) =
            Vector2.toTuple position
    in
        Render.shapeWithOptions Render.rectangle
            { color = color
            , position = ( x, y, 0 )
            , size = ( size, 0.01 )
            , rotation = rotation
            , pivot = ( 0, 0 )
            }
