module EightPuzzle exposing (informed, start)

import Search
import Array2D exposing (Array2D)


type Move
    = Up
    | Down
    | Left
    | Right


type alias State =
    { board : Array2D Int
    , xSize : Int
    , ySize : Int
    }


goalList : Int -> Int -> List Int
goalList x y =
    let
        gen gx gy i =
            if gx > 0 then
                i :: gen (gx - 1) gy (i + 1)
            else if gy > 0 then
                i :: gen x (gy - 1) (i + 1)
            else
                []
    in
        gen x y 0


start : Int -> Int -> State
start x y =
    { board = Array2D.repeat x y 0
    , xSize = x
    , ySize = y
    }


step : Search.Step State
step state =
    [ ( state, True ) ]



{- Packages the search operators. -}


informed : Search.Informed State
informed =
    { step = step
    , cost = \_ -> 1.0
    , heuristic = \_ -> 1.0
    }
