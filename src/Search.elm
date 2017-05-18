module Search
    exposing
        ( Node
        , SearchResult(..)
        , Step
        , Uninformed
        , WithBasicSearch
        , Informed
        , breadthFirst
        , depthFirst
        , aStar
        , greedy
        , depthBounded
        , costBounded
        , fBounded
        , next
        , nextGoal
        )

{-|

# Input types for searches:
@docs Node, Step, WithBasicSearch, Uninformed, Informed

# The search output type:
@docs SearchResult

# Helper functions for iterating searches to produce results:
@docs next, nextGoal

# Uninformed search strategies:
@docs breadthFirst, depthFirst, depthBounded, costBounded

# Informed search strategies:
@docs aStar, greedy, fBounded
-}

import Heap exposing (Heap)


{-| Defines the type of Nodes that searches work over.
-}
type alias Node state =
    ( state, Bool, Int )


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
    an uninformed (non-heuristic) search. This is an extensible record so that
    heuristic searches can also take this type, making it easy to apply switch
    between heuristic and non-heuristic searches.
-}
type alias WithBasicSearch a state =
    { a
        | step : Step state
        , cost : state -> Float
    }


{-| Defines the type of a bundle of operators that need to be supplied to conduct
    an informed (heuristic) search, as an extensible record.
-}
type alias WithHeuristic a state =
    { a | heuristic : state -> Float }


{-| Defines the type of a bundle of operators that need to be supplied to conduct
    an uninformed (non-heuristic) search.
-}
type alias Uninformed state =
    WithBasicSearch {} state


{-| Defines the type of a bundle of operators that need to be supplied to conduct
    an informed (heuristic) search.
-}
type alias Informed state =
    WithHeuristic (WithBasicSearch {} state) state


{-| Defines the type of a function that compares two states and orders them.
-}
type alias Compare state =
    state -> state -> Order


{-| Defines the type of a function that checks if some limit is reached on a
    search node. The most common limit is depth, but other limits are possible.
    The first argument is an Int, and will be passed the iteration numbered from
    zero, when performing an iterative search. For bounds that do not iteratively
    increase, this can be ignored.
-}
type alias Limit state =
    Int -> Node state -> Bool


{-| Defines the operations needed on state buffers that hold the pending search
    states.
-}
type alias Buffer state buffer =
    { orelse : Node state -> buffer -> buffer
    , head : buffer -> Maybe ( Node state, buffer )
    , init : List (Node state) -> buffer
    }


{-| This utility function is used to convert a comparison over states into a
    comparison over search nodes.
-}
nodeCompare : Compare state -> Node state -> Node state -> Order
nodeCompare compare ( state1, _, _ ) ( state2, _, _ ) =
    compare state1 state2


{-| Performs an uninformed search.
-}
search :
    Buffer state buffer
    -> WithBasicSearch a state
    -> Maybe (Limit state)
    -> List (Node state)
    -> SearchResult state
search buffer uninformed maybeLimit start =
    let
        step =
            uninformed.step

        examineHead : buffer -> SearchResult state
        examineHead queue =
            let
                expand depth node queue =
                    (\() ->
                        examineHead <|
                            List.foldl (\( state, isGoal, _ ) queue -> (buffer.orelse ( state, isGoal, depth + 1 ) queue)) queue (step node)
                    )

                notExpand queue =
                    (\() -> examineHead queue)
            in
                case buffer.head queue of
                    Nothing ->
                        Complete

                    Just ( headNode, pendingStates ) ->
                        let
                            d =
                                Debug.log "head" headNode

                            nextStep state depth =
                                case maybeLimit of
                                    Nothing ->
                                        (expand depth ( state, False, depth ) pendingStates)

                                    Just limit ->
                                        if (limit 0 ( state, False, depth )) then
                                            (notExpand pendingStates)
                                        else
                                            (expand depth ( state, False, depth ) pendingStates)
                        in
                            case headNode of
                                ( state, True, depth ) ->
                                    Goal state <| nextStep state depth

                                ( state, False, depth ) ->
                                    Ongoing state <| nextStep state depth
    in
        examineHead <| buffer.init start


{-| Performs an uninformed and unbounded search.
-}
unboundedSearch :
    Buffer state buffer
    -> WithBasicSearch a state
    -> List (Node state)
    -> SearchResult state
unboundedSearch buffer uninformed =
    search buffer uninformed Nothing


{-| Performs an ordered search.
-}
orderedSearch :
    (WithBasicSearch a state -> Compare state)
    -> WithBasicSearch a state
    -> Maybe (Limit state)
    -> List (Node state)
    -> SearchResult state
orderedSearch comparison basicSearch maybeLimit start =
    search (ordered <| comparison basicSearch) basicSearch maybeLimit start


{-| Performs an ordered and unbounded search.
-}
unboundedOrderedSearch :
    (WithBasicSearch a state -> Compare state)
    -> WithBasicSearch a state
    -> List (Node state)
    -> SearchResult state
unboundedOrderedSearch comparison basicSearch =
    search (ordered <| comparison basicSearch) basicSearch Nothing


{-| Implements a first-in first-out buffer using Lists.
-}
fifo : Buffer state (List (Node state))
fifo =
    { orelse = \node list -> node :: list
    , head =
        \list ->
            case list of
                [] ->
                    Nothing

                x :: xs ->
                    Just ( x, xs )
    , init = \list -> list
    }


{-| Implements a last-in first-out buffer using Lists and appending at to the end.
-}
lifo : Buffer state (List (Node state))
lifo =
    { fifo
        | orelse = \node list -> list ++ [ node ]
    }


{-| Implements an order buffer using a heap. A state comparison function is
    supplied to construct the buffer on.
-}
ordered : Compare state -> Buffer state (Heap (Node state))
ordered compare =
    { orelse = \node heap -> Heap.push node heap
    , head = \heap -> Heap.pop heap
    , init = \list -> Heap.fromList (Heap.smallest |> Heap.byCompare (nodeCompare compare)) list
    }


compareH : Informed state -> Compare state
compareH informed =
    \state1 state2 ->
        compare (informed.heuristic state1) (informed.heuristic state2)


compareC : WithBasicSearch a state -> Compare state
compareC informed =
    \state1 state2 ->
        compare (informed.cost state1) (informed.cost state2)


compareF : Informed state -> Compare state
compareF informed =
    \state1 state2 ->
        compare
            (informed.heuristic state1 + informed.cost state1)
            (informed.heuristic state2 + informed.cost state2)


{-| Performs an unbounded depth first search. Depth first searches can easily
    fall into infinite loops.
-}
depthFirst : WithBasicSearch a state -> List (Node state) -> SearchResult state
depthFirst =
    unboundedSearch fifo


{-| Performs an unbounded breadth first search. Breadth first searches store
    a lot of pending nodes in the buffer, so quickly run out of space.
-}
breadthFirst : WithBasicSearch a state -> List (Node state) -> SearchResult state
breadthFirst =
    unboundedSearch lifo


{-| Performs an A* search.  This is one that always follows the search node that
    has the highest f value (f = heuristic + cost).
    The seach will only be optimal if the heuristic function is monotonic.
-}
aStar : Informed state -> List (Node state) -> SearchResult state
aStar =
    unboundedOrderedSearch compareF


{-| Performs a greedy heuristic search.  This is one that always follows the
    search node that has the highest h value (h = heuristic).
-}
greedy : Informed state -> List (Node state) -> SearchResult state
greedy =
    unboundedOrderedSearch compareH


{-| Performs a uniform-cost search. This always follows the search node that
   has the lowest path cost. It is called a uniform cost search because the
   boundary of the search will have a roughly uniform cost as the search
   space is searched by increasing cost.
-}
uniformCost : WithBasicSearch a state -> List (Node state) -> SearchResult state
uniformCost =
    unboundedOrderedSearch compareC


{-| Implements a depth limit on search nodes.
-}
depthLimit : Int -> Limit state
depthLimit maxDepth _ ( _, _, depth ) =
    depth >= maxDepth


{-| Implements a cost limit on search nodes for basic searches.
-}
costLimit : WithBasicSearch a state -> Float -> Limit state
costLimit basicSearch maxCost _ ( state, _, _ ) =
    basicSearch.cost state >= maxCost


{-| Implements an f-limit on search nodes for heuristic searches (f = cost + heuristic)
-}
fLimit : Informed state -> Float -> Limit state
fLimit informed maxF _ ( state, _, _ ) =
    informed.heuristic state + informed.cost state >= maxF


{-| Implements an uninformed search that is bounded to a specified maximum depth.
-}
depthBounded : WithBasicSearch a state -> Int -> List (Node state) -> SearchResult state
depthBounded basicSearch maxDepth =
    search fifo basicSearch (Just <| depthLimit maxDepth)


{-| Implements a cost bounded search. This search will proceed depth first.
-}
costBounded : WithBasicSearch a state -> Float -> List (Node state) -> SearchResult state
costBounded basicSearch maxCost =
    search fifo basicSearch (Just <| costLimit basicSearch maxCost)


{-| Implements a cost bounded search. This search will proceed depth first and
    does not use the heuristic to order search nodes at all.
-}
fBounded : Informed state -> Float -> List (Node state) -> SearchResult state
fBounded informed maxF =
    search fifo informed (Just <| fLimit informed maxF)



-- iterative deepening
-- iterative cost increasing
-- ida-star


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
            cont ()

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
