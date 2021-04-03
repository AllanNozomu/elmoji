module Trie exposing (DataTags, Index, buildIndex, fetchFromIndex)

import Array exposing (Array)
import DiacriticsNormalize exposing (normalize)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Index data =
    { trie : Trie
    , dataDict : Dict Int data
    }


type Trie
    = Empty
    | Trie
        { children : Array Trie
        , data : List Int
        }
    | Leaf
        { data : List Int
        }


type alias Entry data =
    { id : Int
    , tags : List String
    , data : data
    }


type alias DataTags data =
    List ( data, List String )


emptyArray : Array Trie
emptyArray =
    Array.initialize 26 (\_ -> Empty)


indexOfChar : Char -> Int
indexOfChar c =
    Char.toCode c - Char.toCode 'a'


buildIndex : DataTags data -> Index data
buildIndex datas =
    let
        entries =
            List.indexedMap (\index ( data, tags ) -> Entry index tags data) datas

        trie =
            List.foldl
                (\curr acc ->
                    addIntoTrie curr acc
                )
                Empty
                entries

        dataDict =
            List.map (\info -> ( info.id, info.data )) entries
                |> Dict.fromList
    in
    Index trie dataDict


addIntoTrie : Entry x -> Trie -> Trie
addIntoTrie entry trie =
    List.foldl
        (\curr acc ->
            addIntoTrieAux entry.id (normalize curr) acc
        )
        trie
        entry.tags


addIntoTrieAux : Int -> String -> Trie -> Trie
addIntoTrieAux id s trie =
    case String.uncons s of
        Nothing ->
            case trie of
                Empty ->
                    Leaf { data = [ id ] }

                Leaf leaf ->
                    Leaf { leaf | data = id :: leaf.data }

                Trie currTrie ->
                    Trie { currTrie | data = id :: currTrie.data }

        Just ( c, ss ) ->
            let
                index =
                    indexOfChar c
            in
            case trie of
                Empty ->
                    let
                        newChild =
                            addIntoTrieAux id ss Empty

                        newChildren =
                            Array.set index newChild emptyArray
                    in
                    Trie { data = [], children = newChildren }

                Leaf leaf ->
                    let
                        newChild =
                            addIntoTrieAux id ss Empty

                        newChildren =
                            Array.set index newChild emptyArray
                    in
                    Trie { data = leaf.data, children = newChildren }

                Trie currTrie ->
                    let
                        currChild =
                            Maybe.withDefault Empty <| Array.get index currTrie.children

                        newChild =
                            addIntoTrieAux id ss currChild

                        newChildren =
                            Array.set index newChild currTrie.children
                    in
                    Trie { currTrie | children = newChildren }


fetchFromIndex : String -> Index data -> List data
fetchFromIndex s index =
    fetchFromTrie (normalize s |> String.toLower) index.trie
        |> Set.foldl
            (\curr acc ->
                case Dict.get curr index.dataDict of
                    Nothing ->
                        acc

                    Just data ->
                        data :: acc
            )
            []


fetchFromTrie : String -> Trie -> Set Int
fetchFromTrie s trie =
    case trie of
        Empty ->
            Set.empty

        Leaf leaf ->
            case String.uncons s of
                Nothing ->
                    Set.fromList leaf.data

                _ ->
                    Set.empty

        Trie currTrie ->
            case String.uncons s of
                Nothing ->
                    getAllMatches trie

                Just ( c, ss ) ->
                    let
                        index =
                            indexOfChar c
                    in
                    fetchFromTrie ss (Maybe.withDefault Empty <| Array.get index currTrie.children)


getAllMatches : Trie -> Set Int
getAllMatches trie =
    case trie of
        Empty ->
            Set.empty

        Leaf leaf ->
            Set.fromList leaf.data

        Trie currTrie ->
            let
                results =
                    Array.foldl
                        (\curr acc ->
                            Set.union (getAllMatches curr) acc
                        )
                        (Set.fromList currTrie.data)
                        currTrie.children
            in
            results
