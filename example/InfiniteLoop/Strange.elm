module Strange exposing (..)

import Html exposing (text, div)
import Task


type alias Model =
    { count : Int
    }


type Msg
    = Increment Int


init =
    ( { count = 0 }, message <| Increment 0 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "update" msg) of
        Increment val ->
            if model.count < 100000 then
                let
                    nextValue =
                        model.count + 1
                in
                    ( { model | count = nextValue }, message <| Increment nextValue )
            else
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
        [ text (toString (model.count)) ]


message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)
