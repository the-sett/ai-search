module Search
    exposing
        ( Node
        , SearchResult(..)
        , Step
        , Uninformed
        , search
        , next
        , nextGoal
        )

{-|

# Input types for searches:
@docs Node, Step, Uninformed

# The search output type:
@docs SearchResult

# The search function:
@docs search

# Helper functions for iterating searches to produce results:
@docs next, nextGoal
-}


{-| Defines the type of Nodes that searches work over.
-}
type alias Node state =
    ( state, Bool )


{-| Defines the possible outcomes of a search.
-}
type SearchResult state
    = Complete
    | Goal state (() -> SearchResult state)
    | Ongoing state (() -> SearchResult state)


{-| Defines the type of the step function that produces new states from existing
   ones. This is how the graph over the search space is defined.
-}
type alias Step state =
    Node state -> List (Node state)


{-| Defines the type of a bundle of operators that need to be supplied to conduct
   an uninformed (non-heuristic) search.
-}
type alias Uninformed state =
    { step : Step state
    }


{-| Defines the operations needed on state buffers that hold the pending search
    states.
-}
type alias Buffer state buffer =
    { orelse : state -> buffer -> buffer
    , head : buffer -> Maybe state
    , init : List (Node state) -> buffer
    }


{-| Performs an uninformed search.
-}
search : Buffer state buffer -> Uninformed state -> List (Node state) -> SearchResult state
search uninformed start =
    let
        step =
            uninformed.step

        examineHead : List (Node state) -> SearchResult state
        examineHead buffer =
            let
                expand state buffer =
                    (\() ->
                        examineHead <|
                            List.foldl (\node buffer -> buffer ++ [ node ]) buffer (step ( state, False ))
                    )
            in
                case buffer of
                    [] ->
                        Complete

                    ( state, True ) :: pendingStates ->
                        Goal state (expand state pendingStates)

                    ( state, False ) :: pendingStates ->
                        Ongoing (Debug.log "search" state) (expand state pendingStates)
    in
        examineHead start


{-| Steps a search result, to produce the next result.
   * The result of this function may be an Ongoing search. This will provide the
     current head search node and a continuation to run the remainder of the search.
-}
next : SearchResult state -> SearchResult state
next result =
    case result of
        Complete ->
            Complete

        Goal state cont ->
            Goal state cont

        Ongoing _ cont ->
            cont ()


{-| Continues a search result, to produce the next search goal.
   * The result of this function will never be an Ongoing search. This
     function will recursively apply the search until either a Goal state if
     found or the walk over the search space is Complete.
-}
nextGoal : SearchResult state -> SearchResult state
nextGoal result =
    case result of
        Complete ->
            Complete

        Goal state cont ->
            Goal state cont

        Ongoing _ cont ->
            cont () |> nextGoal
