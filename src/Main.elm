port module Main exposing (..)

import Browser
import Emojis exposing (allEmojis, emojisIndex)
import Html exposing (Html, div, input, text, h1, h2)
import Html.Attributes exposing (placeholder, id, class)
import Html.Events exposing (onInput, onClick)
import Trie exposing (Index, fetchFromIndex)


port copy : (String) -> Cmd msg

---- MODEL ----


type alias Model =
    { input : String
    , emojis : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" allEmojis, Cmd.none )



---- UPDATE ----


type Msg
    = ChangeText String
    | Copy String
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText s ->
            let
                emojisList =
                    if String.length s >= 1 then
                        fetchFromIndex s emojisIndex

                    else
                        allEmojis
            in
            ({ model | input = s, emojis = emojisList }, Cmd.none)

        Copy emojiId ->
            (model, copy emojiId)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1[][text "Elmoji"],
        input [ onInput ChangeText, placeholder "Search for Emojis" ] []
        , h2[][text ((String.fromInt (List.length model.emojis)) ++ " emojis found")]
        , div [ id "container"] (List.indexedMap (\index emoji -> (div[class "hover", onClick <| Copy emoji][text emoji])) model.emojis)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
