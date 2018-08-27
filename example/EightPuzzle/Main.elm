module EightPuzzle.Main exposing (main)

import Array exposing (Array)
import EightPuzzle.Search exposing (Previous(..), State, informed, start)
import Html exposing (Html, div, text)
import Random
import Search exposing (SearchResult(..))


main =
    viewResult <|
        Search.nextN 50000 <|
            Search.iterativeDeepeningAStar 5 informed [ start 3 seed ]


viewResult : SearchResult State -> Html Never
viewResult result =
    case result of
        Complete ->
            text "Search space exhausted with no solution found."

        Goal state _ ->
            viewMoves state

        Ongoing state _ ->
            viewMoves state


viewMoves : State -> Html Never
viewMoves state =
    let
        previousMoves state =
            case state.previous of
                None ->
                    [ Html.p [] [ stateToString state |> text ] ]

                Previous prevState ->
                    Html.p [] [ stateToString state |> text ] :: previousMoves prevState
    in
    div [] (previousMoves state |> List.reverse)


stateToString : State -> String
stateToString state =
    (Array.toList state.board |> toString)
        ++ ", distance = "
        ++ toString state.distance
        ++ ", numMoves = "
        ++ toString state.numMoves
        ++ ", lastMove = "
        ++ toString state.lastMove


seed =
    Random.initialSeed 120
