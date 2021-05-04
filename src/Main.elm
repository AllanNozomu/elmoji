port module Main exposing (..)

import Browser
import Emojis exposing (Emoji, EmojiData, SkinTone(..), emojiDataToEmoji)
import Html exposing (Html, div, h1, h2, input, text)
import Html.Attributes exposing (class, id, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder)
import Trie exposing (Index, buildIndex, emptyIndex, fetchFromIndex)


port copy : String -> Cmd msg



---- MODEL ----


type alias Model =
    { input : String
    , emojis : List Emoji
    , allEmojis : List Emoji
    , emojisIndex : Index Emoji
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [] [] emptyIndex, getData )


dataDecoder : Decoder (List EmojiData)
dataDecoder =
    Json.Decode.list <|
        Json.Decode.map4 EmojiData
            (Json.Decode.at [ "code" ] Json.Decode.string)
            (Json.Decode.at [ "keywords" ] (Json.Decode.list Json.Decode.string))
            (Json.Decode.at [ "version" ] Json.Decode.string)
            (Json.Decode.at [ "skinTone" ] Json.Decode.string)


getData : Cmd Msg
getData =
    Http.get
        { url = "emojis.json"
        , expect = Http.expectJson GotData dataDecoder
        }



---- UPDATE ----


type Msg
    = ChangeText String
    | Copy String
    | GotData (Result Http.Error (List EmojiData))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText s ->
            case String.length s of
                0 ->
                    ( { model | input = s, emojis = model.allEmojis }, Cmd.none )

                1 ->
                    ( model, Cmd.none )

                _ ->
                    let
                        emojisList =
                            if String.length s >= 2 then
                                fetchFromIndex s model.emojisIndex

                            else
                                model.allEmojis
                    in
                    ( { model | input = s, emojis = emojisList }, Cmd.none )

        Copy emojiId ->
            ( model, copy emojiId )

        GotData res ->
            case res of
                Ok emojisData ->
                    let
                        emojis =
                            List.map emojiDataToEmoji emojisData

                        emojisKeywords =
                            List.map (\emojiData -> ( emojiDataToEmoji emojiData, emojiData.keywords )) emojisData

                        emojisIndex =
                            buildIndex emojisKeywords
                    in
                    ( { model | emojis = emojis, emojisIndex = emojisIndex, allEmojis = emojis }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elmoji" ]
        , input [ onInput ChangeText, placeholder "Search for Emojis" ] []
        , h2 [] [ text (String.fromInt (List.length model.emojis) ++ " emojis found") ]
        , div [ id "container" ] (List.indexedMap (\index emoji -> div [ class "hover", onClick <| Copy emoji.code ] [ text emoji.code ]) model.emojis)
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
