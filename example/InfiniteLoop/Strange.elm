module Strange exposing (..)

import Html exposing (text, div)
import Task


type alias Model =
    { count : Int
    }


type Msg
    = Count Int


init =
    ( { count = 0 }, message <| Count 0 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "update" msg) of
        Count val ->
            if model.count < 10 then
                ( { model | count = val }, message <| Count (val + 1) )
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
