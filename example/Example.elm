module Example exposing (..)

import Html exposing (text)
import Search
import RiverCrossing


main =
    text <|
        toString <|
            Search.nextGoal <|
                Search.breadthFirst RiverCrossing.uninformed [ ( RiverCrossing.start, False ) ]
