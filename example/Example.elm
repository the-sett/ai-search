module Example exposing (..)

import Html exposing (text)
import Search
import RiverCrossing
import EightPuzzle


-- main =
--     text <|
--         toString <|
--             Search.nextN 250 <|
--                 Search.iterativeDeepening 1 RiverCrossing.uninformed [ RiverCrossing.start ]


main =
    text <|
        toString <|
            Search.nextN 250 <|
                Search.iterativeDeepening 1 EightPuzzle.informed [ EightPuzzle.start 3 3 ]
