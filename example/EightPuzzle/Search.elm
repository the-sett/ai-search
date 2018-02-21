module EightPuzzle.Search exposing (State, Previous(..), informed, start)

{-| Constructs an informed search for the 8-puzzle family.

The Manhatten Heuristic is used.
Immediate move reversals are prevented (up then down, left then right...).

Advanced heuristics such as linear conflict or admissable database heuristics
are not implemented. Linear conflict at least is needed to solve 4x4 puzzles.

An A* search can be used, IDA* will be more efficient.

-}

import Array exposing (Array)
import Search
import Random.List
import Random
import List.Extra exposing (swapAt, elemIndex, zip)
import Lazy.List as LL


{-| The puzzles state
-}
type alias State =
    { board : Array Int -- Current board representation.
    , size : Int -- Board dimension (dimension x dimension).
    , emptyTile : Int -- Empty tile location.
    , distance : Int -- Manhattan distance of the entire board.
    , numMoves : Int -- Number of moves taken to get here.
    , lastMove : Maybe Direction -- The last move taken to get here.
    , previous : Previous -- The state just before this one.
    }


type Previous
    = None
    | Previous State



-- Functions for manipulating the board.


xyToOffset : Int -> Int -> Int -> Int
xyToOffset size x y =
    size * y + x


offsetToXy : Int -> Int -> ( Int, Int )
offsetToXy size index =
    ( index % size, index // size )


{-| Gets the tile at the specified index from the board.
-}
getTileAtIndex : Int -> Array Int -> Int
getTileAtIndex index board =
    Array.get index board |> Maybe.withDefault 0


{-| Checks if a board is solvable. To be solvable:

  - The row number of the empty tile is calculated as `zeroRow` (row index starts from 1).
  - The number of pairs of tiles Ai and Aj such that i < j but Ai > Aj is calculated, as `numberOfInversions`.
  - `zeroRow + numberOfInversions` must be even.

-}
solvable : Int -> List Int -> Bool
solvable size tiles =
    let
        zeroRow =
            1 + (Maybe.withDefault 0 (elemIndex 0 tiles)) // size

        numberOfInversions =
            let
                tilesAndOffset =
                    zip tiles (List.range 0 (size * size)) |> LL.fromList

                inversions =
                    LL.product2 tilesAndOffset tilesAndOffset
                        |> LL.keepIf
                            (\( ( x, xi ), ( y, yi ) ) ->
                                x /= 0 && y /= 0 && yi > xi && x > y
                            )
            in
                LL.length inversions
    in
        (zeroRow + numberOfInversions) % 2 == 0


{-| Swaps the tile at the specified location with the empty tile, to produce a new
puzzle state with its distance metric updated for the swap.
-}
swap : Int -> Int -> State -> State
swap x y state =
    let
        index =
            xyToOffset state.size x y

        newBoard =
            Array.set state.emptyTile (getTileAtIndex index state.board) state.board
                |> Array.set index 0

        newDistance =
            state.distance
                - manhattan (getTileAtIndex index state.board) state.size index
                + manhattan (getTileAtIndex index state.board) state.size state.emptyTile
    in
        { state
            | board = newBoard
            , emptyTile = index
            , distance = newDistance
        }


{-| Calculates the Manhattan distance of a specified tile at a specified location
from its goal location.
-}
manhattan : Int -> Int -> Int -> Int
manhattan tile size index =
    let
        ( x, y ) =
            offsetToXy size index

        ( tileX, tileY ) =
            offsetToXy size (tile - 1)

        xDistance =
            abs (x - tileX)

        yDistance =
            abs (y - tileY)
    in
        if tile == 0 then
            0
        else
            xDistance + yDistance


{-| Calculates the Manhattan distance of all tiles on the board to their goal positions.
-}
distance : Int -> List Int -> Int
distance size board =
    List.indexedMap (\index -> \tile -> manhattan tile size index) board
        |> List.sum


{-| Provides a shuffled board.
Note: this board may not be solvable, additional checks are needed
to see if it is solvable.
-}
shuffled : Int -> Random.Seed -> ( List Int, Random.Seed )
shuffled size seed =
    goalList size
        |> Random.List.shuffle
        |> (flip Random.step) seed


{-| Provides the goal state of the board. This consists of a list of integers numbered
contiguously from zero up to the board size.
-}
goalList : Int -> List Int
goalList size =
    List.range 0 (size * size - 1)


{-| Provides a shuffled board that is also solvable.
The puzzle state is set up from this with the correct size, empty tile position
and distance metric.
-}
start : Int -> Random.Seed -> State
start size seed =
    let
        solvableShuffle size seed =
            let
                ( try, newSeed ) =
                    shuffled size seed
            in
                if solvable size try then
                    try
                else
                    solvableShuffle size newSeed

        board =
            solvableShuffle size seed
    in
        { board = Array.fromList <| board
        , size = size
        , emptyTile = elemIndex 0 board |> Maybe.withDefault 0
        , distance = distance size board
        , numMoves = 0
        , lastMove = Nothing
        , previous = None
        }



-- Board operations and goal checks.


type Direction
    = Up
    | Down
    | Left
    | Right


directions : List Direction
directions =
    [ Up, Down, Left, Right ]


{-| Moves a tile on the board in the specified directon, and checks if the resulting
board is the correct final state of the puzzle.

  - A tile cannot be moved off the edge of the board.
  - A tile will be moved into the empty position only, tiles cannot be swapped
    with other tiles.
  - A tile will not be moved back into the position it was in the previous board
    state, eliminating the worst source of repeated board states.

-}
move : Direction -> State -> Maybe ( State, Bool )
move direction state =
    let
        ( x, y ) =
            offsetToXy state.size state.emptyTile

        previousMoveIs direction state =
            state.lastMove == (Just direction)

        maybeState =
            case direction of
                Up ->
                    if x <= 0 || previousMoveIs Down state then
                        Nothing
                    else
                        Just <| swap (x - 1) y state

                Right ->
                    if y >= state.size - 1 || previousMoveIs Left state then
                        Nothing
                    else
                        Just <| swap x (y + 1) state

                Down ->
                    if x >= state.size - 1 || previousMoveIs Up state then
                        Nothing
                    else
                        Just <| swap (x + 1) y state

                Left ->
                    if y <= 0 || previousMoveIs Right state then
                        Nothing
                    else
                        Just <| swap x (y - 1) state
    in
        maybeState
            |> Maybe.andThen
                (\newState ->
                    Just
                        ( { newState
                            | lastMove = Just direction
                            , previous = Previous state
                            , numMoves = state.numMoves + 1
                          }
                        , goal newState
                        )
                )


{-| Checks if all the tiles are in the correct position.
-}
goal : State -> Bool
goal state =
    state.distance == 0


{-| Produces a list of new board positions that are 1 step away from the specified state,
by trying to apply all of the possible moves.
-}
step : Search.Step State
step node =
    List.filterMap (\direction -> move direction node)
        directions


{-| Packages the search operators as an informed search.
-}
informed : Search.Informed State
informed =
    { step = step
    , cost = \state -> toFloat state.numMoves
    , heuristic = \state -> toFloat state.distance
    }
