module EightPuzzle exposing (informed, start)

import Search
import Random.List
import Random
import List.Extra exposing (swapAt)


type alias State =
    { board : List Int
    , xSize : Int
    , ySize : Int
    }



{- Constructors for goal and random board states. -}


seed =
    Random.initialSeed 10


offset x y width =
    y * width + x


goalList : Int -> Int -> List Int
goalList x y =
    List.range 0 (x * y)


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


swap : Int -> Int -> List a -> List a
swap a b list =
    swapAt a b list
        |> Maybe.withDefault list



{- Board operations and goal checks. -}


type Move
    = Up
    | Down
    | Left
    | Right


move : Move -> State -> Maybe ( State, Bool )
move move state =
    Nothing


goal : State -> Bool
goal state =
    state.board == goalList state.xSize state.ySize


step : Search.Step State
step state =
    [ ( state, True ) ]



{- Packages the search operators as an informed search. -}


informed : Search.Informed State
informed =
    { step = step
    , cost = \_ -> 1.0
    , heuristic = \_ -> 1.0
    }
