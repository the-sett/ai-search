module RiverCrossing exposing (uninformed, start)

import EveryDict as Dict exposing (EveryDict)
import Search
import Maybe.Extra
import Html exposing (text)


main =
    text <|
        toString <|
            Search.nextN 250 <|
                Search.iterativeDeepening 1 uninformed [ start ]



{- Description of the puzzles state. -}


type Character
    = Farmer
    | Wolf
    | Goat
    | Cabbage


type Position
    = West
    | East


type alias State =
    EveryDict Character Position


characters : List Character
characters =
    [ Farmer, Wolf, Goat, Cabbage ]


start : State
start =
    Dict.empty
        |> Dict.insert Farmer West
        |> Dict.insert Wolf West
        |> Dict.insert Goat West
        |> Dict.insert Cabbage West



{- Flips a position between east and west. -}


switch position =
    case position of
        East ->
            West

        West ->
            East



{- Moves the specified character to the opposite bank.
   * The farmer must always be in the boat for the move, so moving a non-farmer
     will automatically include the farmer in the move.
   * When moving a non-farmer the farmer must be on the same side as the thing
     being moved, or else the boat will not be there to make the move.
   * States resulting in the goat or cabbage being eaten will result in Nothing.
   * The goal state where all are on the West bank will be marked as succesfull.
-}


move : Character -> State -> Maybe ( State, Bool )
move character state =
    let
        nextState =
            if character == Farmer then
                Dict.update Farmer (Maybe.map switch) state |> Just
            else
                let
                    farmerPos =
                        Dict.get Farmer state

                    characterPos =
                        Dict.get character state
                in
                    if (farmerPos == characterPos) then
                        Just <|
                            Dict.update Farmer (Maybe.map switch) <|
                                Dict.update character (Maybe.map switch) state
                    else
                        Nothing
    in
        Maybe.Extra.filter (not << illegal) nextState
            |> Maybe.andThen (\state -> Just ( state, goal state ))



{- Checks if a state results in the goat or cabbage being eaten. -}


illegal : State -> Bool
illegal state =
    let
        farmerFlip =
            (Maybe.map switch) <| Dict.get Farmer state

        wolf =
            Dict.get Wolf state

        goat =
            Dict.get Goat state

        cabbage =
            Dict.get Cabbage state
    in
        (farmerFlip == wolf && farmerFlip == goat)
            || (farmerFlip == goat && farmerFlip == cabbage)



{- Checks if a state matches the goal of everthing safely on the East bank. -}


goal : State -> Bool
goal state =
    List.foldl (\character result -> ((Dict.get character state) == Just East) && result) True characters



{- Produces new states from a given state, by attempting to move each of the
   characters in turn to see if that produces a valid new state.
-}


step : Search.Step State
step node =
    List.filterMap (\character -> move character node)
        characters



{- Packages the search as an uninformed search. -}


uninformed : Search.Uninformed State
uninformed =
    { step = step
    , cost = \_ -> 1.0
    }
