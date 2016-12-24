module Main exposing (..)

import Model exposing (..)
import Msg exposing (..)
import GameRender exposing (..)
import Command exposing (..)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Init


init : ( Model, Cmd Msg )
init =
    ( { board = newBoard, command = "" }, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        CommandChange newCommand ->
            ( { model | command = newCommand }, Cmd.none )

        SubmitCommand ->
            let
                _ =
                    Debug.log (toString (findRank model.command)) 1

                _ =
                    Debug.log (toString (findPosition model.command)) 2
            in
                ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ id "game" ]
        [ renderGame model
        , commandForm model
        ]


commandForm : Model -> Html Msg
commandForm model =
    div []
        [ input [ placeholder "Pawn to a7", name "command", type_ "text", onInput CommandChange ] []
        , button [ onClick SubmitCommand ] [ text "Submit" ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }
