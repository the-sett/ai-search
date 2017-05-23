module Search
    exposing
        ( SearchResult(..)
        , Step
        , Uninformed
        , Informed
        , WithBasicSearch
        , breadthFirst
        , depthFirst
        , aStar
        , greedy
        , uniformCost
        , depthBounded
        , costBounded
        , fBounded
        , iterativeDeepening
        , iterativeCostIncreasing
        , next
        , nextN
        , nextGoal
        )

{-|

# Input types for searches:
@docs Step, Uninformed, Informed, WithBasicSearch

# The search output type:
@docs SearchResult

# Helper functions for iterating searches to produce results:
@docs next, nextN, nextGoal

# Uninformed search strategies:
@docs breadthFirst, depthFirst, depthBounded, costBounded, uniformCost,
      iterativeDeepening, iterativeCostIncreasing

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
    state -> List ( state, Bool )


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
    { step : Step state
    , cost : state -> Float
    }


{-| Defines the type of a bundle of operators that need to be supplied to conduct
    an informed (heuristic) search.
-}
type alias Informed state =
    { step : Step state
    , cost : state -> Float
    , heuristic : state -> Float
    }


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


{-| Converts a list of states into search Nodes. It is assumed that the start
    states are never goal states, and are always at depth 0.
-}
makeStartNodes : List state -> List (Node state)
makeStartNodes start =
    List.map (\state -> ( state, False, 0 )) start


{-| Performs an uninformed search.
-}
search :
    Buffer state buffer
    -> WithBasicSearch a state
    -> Maybe (Limit state)
    -> Int
    -> List state
    -> SearchResult state
search buffer uninformed maybeLimit iteration start =
    let
        step =
            uninformed.step

        examineHead : buffer -> SearchResult state
        examineHead queue =
            let
                expand depth state queue =
                    (\() ->
                        examineHead <|
                            List.foldl (\( state, isGoal ) queue -> (buffer.orelse ( state, isGoal, depth + 1 ) queue)) queue (step state)
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
                                        (expand depth state pendingStates)

                                    Just limit ->
                                        if (limit iteration ( state, False, depth )) then
                                            (notExpand pendingStates)
                                        else
                                            (expand depth state pendingStates)
                        in
                            case headNode of
                                ( state, True, depth ) ->
                                    Goal state <| nextStep state depth

                                ( state, False, depth ) ->
                                    Ongoing state <| nextStep state depth
    in
        examineHead <| buffer.init (makeStartNodes start)


{-| Performs an uninformed and unbounded search.
-}
unboundedSearch :
    Buffer state buffer
    -> WithBasicSearch a state
    -> List state
    -> SearchResult state
unboundedSearch buffer uninformed =
    search buffer uninformed Nothing 0


{-| Performs an ordered search.
-}
orderedSearch :
    (WithBasicSearch a state -> Compare state)
    -> WithBasicSearch a state
    -> Maybe (Limit state)
    -> List state
    -> SearchResult state
orderedSearch comparison basicSearch maybeLimit =
    search (ordered <| comparison basicSearch) basicSearch maybeLimit 0


{-| Performs an ordered and unbounded search.
-}
unboundedOrderedSearch :
    (WithBasicSearch a state -> Compare state)
    -> WithBasicSearch a state
    -> List state
    -> SearchResult state
unboundedOrderedSearch comparison basicSearch =
    search (ordered <| comparison basicSearch) basicSearch Nothing 0


{-| Performs an iterative search. Every time the search reaches Complete
    (due to the specified limit being reach), a new search is started from the
    beginning at the next iteration.
-}
iterativeSearch :
    Buffer state buffer
    -> WithBasicSearch a state
    -> Limit state
    -> List state
    -> SearchResult state
iterativeSearch buffer basicSearch limit start =
    let
        iteration count =
            evaluate count (search buffer basicSearch (Just limit) count start)

        evaluate count result =
            case result of
                Complete ->
                    iteration (count + 1)

                Goal state cont ->
                    Goal state cont

                Ongoing state cont ->
                    Ongoing state (\() -> evaluate count (cont ()))
    in
        iteration 0


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
depthFirst : WithBasicSearch a state -> List state -> SearchResult state
depthFirst =
    unboundedSearch fifo


{-| Performs an unbounded breadth first search. Breadth first searches store
    a lot of pending nodes in the buffer, so quickly run out of space.
-}
breadthFirst : WithBasicSearch a state -> List state -> SearchResult state
breadthFirst =
    unboundedSearch lifo


{-| Performs an A* search.  This is one that always follows the search node that
    has the highest f value (f = heuristic + cost).
    The seach will only be optimal if the heuristic function is monotonic.
-}
aStar : Informed state -> List state -> SearchResult state
aStar =
    unboundedOrderedSearch compareF


{-| Performs a greedy heuristic search.  This is one that always follows the
    search node that has the highest h value (h = heuristic).
-}
greedy : Informed state -> List state -> SearchResult state
greedy =
    unboundedOrderedSearch compareH


{-| Performs a uniform-cost search. This always follows the search node that
   has the lowest path cost. It is called a uniform cost search because the
   boundary of the search will have a roughly uniform cost as the search
   space is searched by increasing cost.
-}
uniformCost : WithBasicSearch a state -> List state -> SearchResult state
uniformCost =
    unboundedOrderedSearch compareC


{-| Implements a depth limit on search nodes. This is a fixed limit, not iterative.
-}
depthLimit : Int -> Limit state
depthLimit maxDepth _ ( _, _, depth ) =
    depth >= maxDepth


{-| Implements a cost limit on search nodes for basic searches. This is a fixed
    limit, not iterative.
-}
costLimit : WithBasicSearch a state -> Float -> Limit state
costLimit basicSearch maxCost _ ( state, _, _ ) =
    basicSearch.cost state >= maxCost


{-| Implements an f-limit on search nodes for heuristic searches (f = cost + heuristic).
    This is a fixed limit, not iterative.
-}
fLimit : Informed state -> Float -> Limit state
fLimit informed maxF _ ( state, _, _ ) =
    informed.heuristic state + informed.cost state >= maxF


{-| Implements an uninformed search that is bounded to a specified maximum depth.
-}
depthBounded : WithBasicSearch a state -> Int -> List state -> SearchResult state
depthBounded basicSearch maxDepth =
    search fifo basicSearch (Just <| depthLimit maxDepth) 0


{-| Implements a cost bounded search. This search will proceed depth first.
-}
costBounded : WithBasicSearch a state -> Float -> List state -> SearchResult state
costBounded basicSearch maxCost =
    search fifo basicSearch (Just <| costLimit basicSearch maxCost) 0


{-| Implements a cost bounded search. This search will proceed depth first and
    does not use the heuristic to order search nodes at all.
-}
fBounded : Informed state -> Float -> List state -> SearchResult state
fBounded informed maxF =
    search fifo informed (Just <| fLimit informed maxF) 0


{-| Implements a depth limit on search nodes. This is an iterative limit. The
    iteration number is multiplied by a specified multiple to calculate the
    maximum depth allowed at a given iteration.
-}
iterativeDepthLimit : Int -> Limit state
iterativeDepthLimit multiple iteration ( _, _, depth ) =
    depth >= (iteration + 1) * multiple


{-| Implements a cost limit on search nodes for basic searches. This is an
    iterative limit. The iteration number is multiplied by a specified multiple
    to calculate the maximum cost allowed at a given iteration.
-}
iterativeCostLimit : WithBasicSearch a state -> Float -> Limit state
iterativeCostLimit basicSearch multiple iteration ( state, _, _ ) =
    basicSearch.cost state >= toFloat (iteration + 1) * multiple


{-| Implements an iterative deepening search. This search proceed depth first
    but repeats at progressively larger depth limits. The iteration number is
    multiplied by a specified multiple to calculate the maximum depth allowed
    at a given iteration.
-}
iterativeDeepening : Int -> WithBasicSearch a state -> List state -> SearchResult state
iterativeDeepening multiple basicSearch =
    iterativeSearch fifo basicSearch (iterativeDepthLimit multiple)


{-| Implements an iterative cost increasing. This search proceed depth first
    but repeats at progressively larger cost limits. The iteration number is
    multiplied by a specified multiple to calculate the maximum cost allowed
    at a given iteration.
-}
iterativeCostIncreasing : Float -> WithBasicSearch a state -> List state -> SearchResult state
iterativeCostIncreasing multiple basicSearch =
    iterativeSearch fifo basicSearch (iterativeCostLimit basicSearch multiple)



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


{-| Continues a search result, to produce the next search goal up to a limited
    number of iterations.
   * This function will recursively apply the search until either a Goal state
     is found or the walk over the search space is Complete, or the iteration
     count is exhausted in which case an Ongoing search may be returned.
-}
nextN : Int -> SearchResult state -> SearchResult state
nextN count result =
    case result of
        Complete ->
            Complete

        Goal state cont ->
            Goal state cont

        Ongoing _ cont ->
            if count > 0 then
                cont () |> nextN (count - 1)
            else
                cont ()


{-| Continues a search result, to produce the next search goal.
   * The result of this function will never be an Ongoing search. This
     function will recursively apply the search until either a Goal state is
     found or the walk over the search space is Complete.
   * If the search is bad and no goal can ever be found, this function may
     infnite loop.
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
