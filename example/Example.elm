module Example exposing (..)

import Html exposing (text)
import Search
import RiverCrossing


main =
    text <|
        toString <|
            Search.nextGoal <|
                Search.breadthFirstSearch RiverCrossing.uninformed [ ( RiverCrossing.start, False ) ]
