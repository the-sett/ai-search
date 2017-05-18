module Example exposing (..)

import Html exposing (text)
import Search
import RiverCrossing


main =
    text <|
        toString <|
            Search.nextGoal <|
                Search.depthBounded RiverCrossing.uninformed 7 [ ( RiverCrossing.start, False, 0 ) ]
