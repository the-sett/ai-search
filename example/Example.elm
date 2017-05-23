module Example exposing (..)

import Html exposing (text)
import Search
import RiverCrossing


main =
    text <|
        toString <|
            Search.nextN 250 <|
                Search.iterativeDeepening 1 RiverCrossing.uninformed [ RiverCrossing.start ]
