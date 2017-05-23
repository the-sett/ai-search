module Example exposing (..)

import Html exposing (text)
import Search exposing (..)
import RiverCrossing


main =
    text <|
        toString <|
            nextN 250 <|
                iterativeDeepening 1 RiverCrossing.uninformed [ RiverCrossing.start ]
