module Main exposing (..)

import Search
import Html exposing (text)


main =
    text "infinite loop timer"


type alias State =
    Int


step : Search.Step State
step state =
    [ ( state + 1, False ) ]


{-| Packages the infinite loop as an uninformed search.
-}
uninformed : Search.Uninformed State
uninformed =
    { step = step
    , cost = \_ -> 1.0
    }
