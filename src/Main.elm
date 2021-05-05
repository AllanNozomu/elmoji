port module Main exposing (..)

import Browser
import Emojis exposing (Emoji, EmojiData, emojiDataToEmoji)
import Html exposing (Html, div, h1, h2, input, text)
import Html.Attributes exposing (checked, class, id, placeholder, type_)
import Html.Events exposing (onCheck, onClick, onInput)
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
    , include12_1 : Bool
    , include13_0 : Bool
    , include13_1 : Bool
    , skinFilters : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [] [] emptyIndex Loading True True True [ 1, 2, 3, 4, 5 ], getData )


dataDecoder : Decoder (List EmojiData)
dataDecoder =
    Json.Decode.list <|
        Json.Decode.map4 EmojiData
            (Json.Decode.at [ "code" ] Json.Decode.string)
            (Json.Decode.at [ "keywords" ] (Json.Decode.list Json.Decode.string))
            (Json.Decode.at [ "version" ] Json.Decode.string)
            (Json.Decode.at [ "skinTone" ] (Json.Decode.list Json.Decode.int))


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
    | ToggleVersion Int Bool
    | ToggleSkin Int Bool


filterVersions : Model -> List Emoji -> List Emoji
filterVersions model emojis =
    List.filter
        (\emoji ->
            if model.include12_1 then
                True

            else
                not model.include12_1 && emoji.version /= "12.1"
        )
        emojis
        |> List.filter
            (\emoji ->
                if model.include13_0 then
                    True

                else
                    not model.include13_0 && emoji.version /= "13.0"
            )
        |> List.filter
            (\emoji ->
                if model.include13_1 then
                    True

                else
                    not model.include13_1 && emoji.version /= "13.1"
            )


filterSkins : Model -> List Emoji -> List Emoji
filterSkins model emojis =
    List.filter
        (\emoji ->
            List.foldl
                (\curr acc ->
                    acc && List.member curr model.skinFilters
                )
                True
                emoji.skinTone
        )
        emojis


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText s ->
            let
                emojisList =
                    if String.length s == 0 then
                        model.allEmojis

                    else
                        fetchFromIndex s model.emojisIndex

                filteredEmojis =
                    filterVersions model emojisList |> filterSkins model
            in
            ( { model | input = s, emojis = filteredEmojis }, Cmd.none )

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

        ToggleVersion version toggle ->
            let
                newModel =
                    case version of
                        121 ->
                            { model | include12_1 = toggle }

                        130 ->
                            { model | include13_0 = toggle }

                        131 ->
                            { model | include13_1 = toggle }

                        _ ->
                            model
            in
            update (ChangeText model.input) newModel

        ToggleSkin skin toggle ->
            let
                newSkins =
                    if toggle then
                        skin :: model.skinFilters

                    else
                        List.filter (\s -> s /= skin) model.skinFilters
            in
            update (ChangeText model.input) { model | skinFilters = newSkins }



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
            , div []
                [ text "Include versions:"
                , input [ type_ "checkbox", checked model.include12_1, onCheck <| ToggleVersion 121 ] []
                , text "12.1"
                , input [ type_ "checkbox", checked model.include13_0, onCheck <| ToggleVersion 130 ] []
                , text "13.0"
                , input [ type_ "checkbox", checked model.include13_1, onCheck <| ToggleVersion 131 ] []
                , text "13.1"
                ]
            , div []
                [ text "Skins:"
                , input [ type_ "checkbox", checked <| List.member 1 model.skinFilters, onCheck <| ToggleSkin 1 ] []
                , text "🖐🏻"
                , input [ type_ "checkbox", checked <| List.member 2 model.skinFilters, onCheck <| ToggleSkin 2 ] []
                , text "🖐🏼"
                , input [ type_ "checkbox", checked <| List.member 3 model.skinFilters, onCheck <| ToggleSkin 3 ] []
                , text "🖐🏽"
                , input [ type_ "checkbox", checked <| List.member 4 model.skinFilters, onCheck <| ToggleSkin 4 ] []
                , text "🖐🏾"
                , input [ type_ "checkbox", checked <| List.member 5 model.skinFilters, onCheck <| ToggleSkin 5 ] []
                , text "🖐🏿"
                ]
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
