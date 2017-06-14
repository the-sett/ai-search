module EightPuzzle exposing (informed, start)

import Search
import Random.List
import Random
import List.Extra exposing (swapAt)


type Move
    = Up
    | Down
    | Left
    | Right


type alias State =
    { board : List Int
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


seed =
    Random.initialSeed 10


offset x y width =
    y * width + x


swap : Int -> Int -> List a -> List a
swap a b list =
    swapAt a b list
        |> Maybe.withDefault list


shuffled : Int -> Int -> List Int
shuffled x y =
    goalList x y
        |> Random.List.shuffle
        |> (flip Random.step) seed
        |> Tuple.first


start : Int -> Int -> State
start x y =
    { board = shuffled x y
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
