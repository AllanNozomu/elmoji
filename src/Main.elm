module Main exposing (..)

import Browser
import Emojis exposing (allEmojis, emojisIndex)
import Html exposing (Html, div, input, text, h1)
import Html.Attributes exposing (placeholder, id, class)
import Html.Events exposing (onInput)
import Trie exposing (Index, fetchFromIndex)


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



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1[][text "Elmoji"],
        input [ onInput ChangeText, placeholder "Search for Emojis" ] []
        , div[][text ((String.fromInt (List.length model.emojis)) ++ " emojis found")]
        , div [ id "container"] (List.map (\emoji -> (div[][text emoji])) model.emojis)
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
