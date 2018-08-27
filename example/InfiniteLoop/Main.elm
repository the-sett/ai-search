module Main exposing (Model, Msg(..), State, init, main, step, uninformed, update, view)

import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Search
import Time exposing (Time)


type alias Model =
    { state : State
    , running : Bool
    , startTime : Int
    , endTime : Int
    }


type Msg
    = Run
    | Pause
    | StartTime Time
    | EndTime Time


init =
    ( { state = 0
      , running = False
      , startTime = 0
      , endTime = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        Run ->
            ( model, Cmd.none )

        Pause ->
            ( model, Cmd.none )

        StartTime time ->
            ( model, Cmd.none )

        EndTime time ->
            ( model, Cmd.none )


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


view model =
    div []
        [ if model.running then
            button [ onClick Pause ] [ text "pause" ]

          else
            button [ onClick Run ] [ text "start" ]
        , text (toString model.state)
        ]



-- An infinite looping search space, just incremenets a counter.


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
