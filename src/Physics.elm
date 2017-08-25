module Physics exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


-- MODEL


type alias Physical r =
    { r
        | position : Vec2
        , rotation : Float
        , velocity : Vec2
        , acceleration : Vec2
        , mass : Float
        , forces : Dict String Vec2
    }


setForce : String -> Vec2 -> Physical r -> Physical r
setForce name force p =
    { p | forces = Dict.insert name force p.forces }


removeForce : String -> Physical r -> Physical r
removeForce name p =
    { p | forces = Dict.remove name p.forces }


removeVelocity : Physical r -> Physical r
removeVelocity p =
    { p | velocity = vec2 0 0 }



-- UPDATE


update : Float -> Physical r -> Physical r
update dt p =
    p
        |> force dt
        |> accelerate
        |> move


force : Float -> Physical r -> Physical r
force dt p =
    let
        applyForce ( _, force ) acc =
            Vec2.add acc (Vec2.scale (dt * 0.001) force)
    in
        { p
            | acceleration =
                p.forces
                    |> Dict.toList
                    |> List.foldl applyForce (vec2 0 0)
        }


accelerate : Physical r -> Physical r
accelerate p =
    { p | velocity = Vec2.add p.velocity p.acceleration }


move : Physical r -> Physical r
move p =
    { p | position = Vec2.add p.position p.velocity }



-- UTILS


toHeading : Float -> Vec2
toHeading rotation =
    let
        x =
            (cos rotation) - (sin rotation)

        y =
            (sin rotation) + (cos rotation)
    in
        vec2 (cos (rotation + (degrees 90))) (sin (rotation + (degrees 90)))


toRotation : Vec2 -> Float
toRotation heading =
    atan2 (Vec2.getY heading) (Vec2.getX heading)


attract : Vec2 -> Vec2 -> Vec2
attract =
    Vec2.sub


repulse : Vec2 -> Vec2 -> Vec2
repulse source target =
    attract target source
