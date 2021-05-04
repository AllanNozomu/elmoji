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


type Status
    = Failure
    | Loading
    | Success


type alias Model =
    { input : String
    , emojis : List Emoji
    , allEmojis : List Emoji
    , emojisIndex : Index Emoji
    , status : Status
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [] [] emptyIndex Loading, getData )


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
            if String.length s == 0 then
                ( { model | input = s, emojis = model.allEmojis }, Cmd.none )

            else
                let
                    emojisList =
                        fetchFromIndex s model.emojisIndex
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
                    ( { model | status = Success, emojis = emojis, emojisIndex = emojisIndex, allEmojis = emojis }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    if model.status == Loading then
        div []
            [ h1 [] [ text "Elmoji" ]
            , h2 [] [ text "Carregando" ]
            , div [ class "loader-inner ball-pulse" ]
                [ div [] []
                , div [] []
                , div [] []
                ]
            ]

    else
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
