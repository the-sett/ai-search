module Main exposing (..)

import Html exposing (text, div, button)
import Html.Events exposing (onClick)
import Search exposing (SearchResult(..))
import Task
import Time exposing (Time)


type alias Model =
    { state : State
    , running : Bool
    , startTime : Time
    , endTime : Time
    }


type Msg
    = Run
    | Pause
    | StartTime Time
    | EndTime Time
    | Compute (SearchResult State)


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
    case (Debug.log "update" msg) of
        Run ->
            ( { model | running = True, state = 0 }, Task.perform StartTime Time.now )

        Pause ->
            ( { model | running = False }, Task.perform EndTime Time.now )

        StartTime time ->
            ( { model | startTime = time, endTime = 0 }
            , message <|
                Compute <|
                    Search.depthFirst uninformed [ 0 ]
            )

        EndTime time ->
            ( { model | endTime = time }, Cmd.none )

        Compute result ->
            case result of
                Complete ->
                    Debug.crash "Search space should not complete."

                Goal state more ->
                    Debug.crash "Search space should never find a goal."

                Ongoing state more ->
                    if model.running && model.state <= 100000000 then
                        ( { model | state = state }, message <| Compute <| Search.nextN 99 result )
                    else if model.running then
                        ( { model | state = state }, message Pause )
                    else
                        ( { model | state = state }, Cmd.none )


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


br =
    Html.br [] []


view model =
    div []
        [ if model.running then
            button [ onClick Pause ] [ text "pause" ]
          else
            button [ onClick Run ] [ text "start" ]
        , br
        , text (toString (model.state))
        , br
        , if model.startTime /= 0 && model.endTime /= 0 then
            text "results"
          else
            text "no results"
        ]


message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)



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
