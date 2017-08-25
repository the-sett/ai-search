module Heap
    exposing
        ( Heap
        , Options
        , smallest
        , biggest
        , by
        , byCompare
        , thenBy
        , empty
        , singleton
        , fromList
        , isEmpty
        , size
        , peek
        , push
        , pop
        , popBlind
        , mergeInto
        , toList
        , toListReverse
        , toListUnordered
        )

{-| Data structure for heaps.
This package exposes a data structure to implement heaps/priority queues/fast
in-place sorting.
The heap is implemented as a pairing heap, as it is simple but fast, and has
been shown to work well in real-world situations.


# Definition

@docs Heap, Options, smallest, biggest, by, thenBy


# Creating heaps

@docs empty, singleton, fromList


# Inserting/removing values

@docs push, mergeInto, pop, popBlind


# Inspecting heaps

@docs isEmpty, size, peek


# Converting to lists

@docs toList, toListReverse, toListUnordered


# Running times

  - peek: **Θ(1)**
  - pop: **O(log n) (amortized)**
  - push: **Θ(1)**
  - size: **Θ(1)**
  - mergeInto: **Θ(1)**

-}


{-| A heap `Heap  a` takes values of type `a`, keeping them loosely ordered.
Values can be very quickly added, and, depending on the type of heap, either the
"smallest" or "biggest" value can be quickly recalled or removed.
-}
type Heap a
    = Heap (Model a)


type alias Model a =
    { structure : Node a
    , size : Int
    , compare : SortOrder -> a -> a -> Order
    , order : SortOrder
    }


type Node a
    = Branch a (List (Node a))
    | Leaf


type SortOrder
    = MinFirst
    | MaxFirst


{-| When creating a new heap `Heap a`, `Options a` must be provided. They will
determine whether the heap keeps the "smallest" or "biggest" value to hand, and
how it determines how small or big the value is.
-}
type Options a
    = Options
        { order : SortOrder
        , compare : a -> a -> Order
        }


{-| A `smallest` heap is a heap of any comparable type (ints, floats, chars, strings,
lists, or tuples), which keeps the smallest value to hand.
>>> Heap.fromList smallest [ 0, 1, 2, 3, 4 ]
... |> Heap.peek
Just 0
-}
smallest : Options comparable
smallest =
    Options
        { order = MinFirst
        , compare = Basics.compare
        }


{-| A `biggest` heap is a heap of any comparable type (ints, floats, chars, strings,
lists, or tuples), which keeps the biggest value to hand.
>>> Heap.fromList biggest [ 0, 1, 2, 3, 4 ]
... |> Heap.peek
Just 4
-}
biggest : Options comparable
biggest =
    Options
        { order = MaxFirst
        , compare = Basics.compare
        }


{-| `by someFunction` tells the heap to sort by comparing values with
the given function. This may commonly be a property of a record:
Heap.singleton (biggest |> by .yearOfBirth)
{ firstName = "Buzz"
, lastName = "Aldrin"
, yearOfBirth = 1930
}
… or a hashing/consolidation function:
Heap.singleton (biggest |> by List.length)
[ 1, 2, 3, 4, 5, 6 ]
-}
by : (a -> comparable) -> Options b -> Options a
by hash (Options options) =
    Options
        { options
            | compare = makeCompare hash
        }


byCompare : (a -> a -> Order) -> Options b -> Options a
byCompare compare (Options options) =
    Options
        { options
            | compare = compare
        }


{-| `thenBy  someFunction` tells the heap to use the given function to compare
values, if it cannot otherwise differentiate between two values.
Heap.singleton (smallest |> by .lastName |> thenBy .firstName)
{ firstName = "Buzz"
, lastName = "Aldrin"
, yearOfBirth = 1930
}
-}
thenBy : (a -> comparable) -> Options a -> Options a
thenBy hash (Options options) =
    Options
        { options
            | compare = options.compare |> ifEQ (makeCompare hash)
        }


{-| Given Heap.Options, returns an empty heap.
Heap.empty smallest
|> Heap.push 376373
Heap.empty (smallest |> by .age)
|> Heap.push { firstName = "Pippi", lastName = "Longstocking", age = 9 }
-}
empty : Options a -> Heap a
empty (Options { compare, order }) =
    Heap
        { structure = Leaf
        , size = 0
        , compare = makeReversible compare
        , order = order
        }


{-| A heap containing one value, given Heap.Options
Heap.singleton (smallest |> by .age)
{ firstName = "Pippi", lastName = "Longstocking", age = 9 }
Heap.singleton biggest
"Peter Piper picked a pack of pickled peppers"
Heap.singleton (biggest |> by String.length)
"Peter Piper picked a pack of pickled peppers"
-}
singleton : Options a -> a -> Heap a
singleton (Options { compare, order }) value =
    Heap
        { structure = Branch value []
        , size = 1
        , compare = makeReversible compare
        , order = order
        }


{-| A heap containing all values in the list, given Heap.Options.
>>> Heap.fromList (biggest |> by (List.maximum >> Maybe.withDefault -999999))
... [ [ 1, 999 ]
... , [ 6, 4, 3, 8, 9, 347, 34, 132, 546 ]
... ][ [ 1, 999 ]
...     , [ 6, 4, 3, 8, 9, 347, 34, 132, 546 ]
...     ]
... |> Heap.peek
Just [ 1, 999 ]
>>> Heap.fromList smallest []
... |> Heap.size
0
>>> Heap.fromList smallest [ 8, 3, 8, 3, 6, 67, 23 ]
... |> Heap.size
7
-}
fromList : Options a -> List a -> Heap a
fromList =
    List.foldl push << empty


{-| `True` if the Heap is empty, otherwise `False`.
>>> Heap.isEmpty (Heap.empty smallest)
True
>>> Heap.isEmpty (Heap.singleton smallest 3)
False
-}
isEmpty : Heap a -> Bool
isEmpty (Heap { size }) =
    size == 0


{-| Number of elements in heap.
>>> Heap.size (Heap.empty biggest)
0
>>> Heap.size (Heap.fromList biggest [ 1, 2, 3, 4, 5, 6, 7, 8 ])
8
-}
size : Heap a -> Int
size (Heap h) =
    h.size


{-| Look at smallest/biggest value in heap without applying any transformations.
>>> Heap.peek (Heap.empty smallest)
Nothing
>>> Heap.peek (Heap.fromList smallest [ 3, 56, 8, 367, 0, 4 ])
Just 0
>>> Heap.peek (Heap.fromList biggest [ 3, 56, 8, 367, 0, 4 ])
Just 367
-}
peek : Heap a -> Maybe a
peek (Heap { structure }) =
    case structure of
        Leaf ->
            Nothing

        Branch a _ ->
            Just a


{-| Add a value to a heap.
>>> Heap.fromList smallest [ 1, 6, 7 ]
... |> Heap.push 4
... |> Heap.peek
Just 1
>>> Heap.fromList smallest [ 5, 6, 7 ]
... |> Heap.push 4
... |> Heap.peek
Just 4
-}
push : a -> Heap a -> Heap a
push a (Heap heap) =
    mergeInto (Heap heap) (Heap { heap | structure = Branch a [], size = 1 })


{-| Try to remove the top value from the heap, returning the value and the
new heap. If the heap is empty, return Nothing.
>>> Heap.pop (Heap.empty biggest)
Nothing
>>> Heap.fromList smallest [ 3, 5, 7, 7, 2, 9 ]
... |> Heap.pop
... |> Maybe.map (Tuple.mapSecond Heap.size)
Just (2, 5)
-}
pop : Heap a -> Maybe ( a, Heap a )
pop (Heap heap) =
    case heap.structure of
        Leaf ->
            Nothing

        Branch a subheap ->
            Just ( a, Heap { heap | structure = mergePairs heap subheap, size = heap.size - 1 } )


{-| Try to remove the top value from the heap, returning just the new heap.
If the heap is empty, return Nothing.
>>> Heap.popBlind (Heap.empty smallest)
Nothing
>>> Heap.singleton smallest 3
... |> Heap.popBlind
... |> Maybe.map Heap.size
Just 0
-}
popBlind : Heap a -> Maybe (Heap a)
popBlind =
    Maybe.map Tuple.second << pop


{-| Merge the second heap into the first heap.
**Note** This function assumes that both heaps are sorted using the same method.
Strictly speaking, the merged heap has the same sorting method as the first heap
given.
>>> Heap.isEmpty (Heap.mergeInto (Heap.empty smallest) (Heap.empty smallest))
True
>>> Heap.mergeInto (Heap.fromList smallest [ 2, 4, 6, 7 ]) (Heap.fromList smallest [ 5, 7, 9, 3 ])
... |> Heap.size
8
-}
mergeInto : Heap a -> Heap a -> Heap a
mergeInto (Heap heap) (Heap toMerge) =
    Heap <|
        case heap.structure of
            Leaf ->
                { heap
                    | structure = toMerge.structure
                    , size = toMerge.size
                }

            Branch elem1 subheap1 ->
                case toMerge.structure of
                    Leaf ->
                        heap

                    Branch elem2 subheap2 ->
                        if heap.compare heap.order elem1 elem2 == LT then
                            { heap
                                | structure = Branch elem1 (toMerge.structure :: subheap1)
                                , size = heap.size + toMerge.size
                            }
                        else
                            { heap
                                | structure = Branch elem2 (heap.structure :: subheap2)
                                , size = heap.size + toMerge.size
                            }


{-| Get all values from the heap, in order.
>>> Heap.toList (Heap.fromList smallest [ 9, 3, 6, 4, 1, 2, 8, 5, 7 ])
[ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
-}
toList : Heap a -> List a
toList =
    List.reverse << toListReverse


{-| Get all values from the heap, in reverse order.
>>> Heap.toListReverse (Heap.fromList smallest [ 9, 3, 6, 4, 1, 2, 8, 5, 7 ])
[ 9, 8, 7, 6, 5, 4, 3, 2, 1 ]
-}
toListReverse : Heap a -> List a
toListReverse =
    let
        toListHelper popped heap =
            case pop heap of
                Nothing ->
                    popped

                Just ( el, subheap ) ->
                    toListHelper (el :: popped) subheap
    in
        toListHelper []


{-| Get all values out as fast as possible, regardless of order
-}
toListUnordered : Heap a -> List a
toListUnordered (Heap { structure }) =
    flattenStructure structure


flattenStructure : Node a -> List a
flattenStructure nodes =
    case nodes of
        Leaf ->
            []

        Branch a rest ->
            a :: List.concat (List.map flattenStructure rest)


mergePairs : Model a -> List (Node a) -> Node a
mergePairs heap nodes =
    case List.filter ((/=) Leaf) nodes of
        [] ->
            Leaf

        node :: [] ->
            node

        node1 :: node2 :: rest ->
            let
                (Heap { structure }) =
                    mergeInto
                        (mergeInto (Heap { heap | structure = node1 }) (Heap { heap | structure = node2 }))
                        (Heap { heap | structure = mergePairs heap rest })
            in
                structure


makeReversible : (a -> a -> Order) -> (SortOrder -> a -> a -> Order)
makeReversible compare order =
    case order of
        MinFirst ->
            compare

        MaxFirst ->
            reverseCompare compare


reverseCompare : (a -> a -> Order) -> (a -> a -> Order)
reverseCompare fn a b =
    case fn a b of
        GT ->
            LT

        LT ->
            GT

        EQ ->
            EQ


makeCompare : (a -> comparable) -> (a -> a -> Order)
makeCompare fn a b =
    Basics.compare (fn a) (fn b)


ifEQ : (a -> a -> Order) -> (a -> a -> Order) -> (a -> a -> Order)
ifEQ second first a b =
    let
        firstComparison =
            first a b
    in
        if firstComparison == EQ then
            second a b
        else
            firstComparison
