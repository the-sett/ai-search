module Example exposing (..)

import Html exposing (text)
import Search
import RiverCrossing


main =
    text <|
        toString <|
            Search.nextGoal <|
                Search.iterativeDeepening 1 RiverCrossing.uninformed [ ( RiverCrossing.start, False, 0 ) ]
