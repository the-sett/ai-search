module SearchTrial exposing (..)

import Html exposing (text)
import Search
import RiverCrossing


main =
    text <| toString <| Search.nextGoal <| Search.search RiverCrossing.uninformed [ ( RiverCrossing.start, False ) ]
