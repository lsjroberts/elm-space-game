module Physics exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


type alias Physical r =
    { r
        | position : Vec2
        , rotation : Float
        , velocity : Vec2
        , acceleration : Vec2
        , forces : Dict String Vec2
    }



-- physical position rotation r =
--     { r
--         | position = position
--         , rotation = rotation
--         , velocity = vec2 0 0
--         , acceleration = vec2 0 0
--         , forces = Dict.fromList []
--     }


setForce : String -> Vec2 -> Physical r -> Physical r
setForce name force p =
    { p | forces = Dict.insert name force p.forces }


update : Float -> Physical r -> Physical r
update dt p =
    p
        |> accelerate
        |> move


accelerate : Physical r -> Physical r
accelerate p =
    p


move : Physical r -> Physical r
move p =
    p
