module Book exposing (..)

import Array exposing (Array)
import Char exposing (Char)
import Json.Decode as D


type alias Book =
    { name : String
    , author : String
    , chapters : Array Chapter
    , bookmark : Bookmark
    }


type alias Chapter =
    { name : String
    , content : Array String
    }


type alias Bookmark =
    { chapter : Int
    , word : Int
    , letter : Int
    }


bookDecoder : D.Decoder Book
bookDecoder =
    D.map4 Book
        (D.field "name" D.string)
        --name
        (D.field "author" D.string)
        --author
        (D.field "chapters" chapterDecoder)
        --chapter
        (D.succeed (Bookmark 0 0 0))



--bookmark


chapterDecoder : D.Decoder (Array Chapter)
chapterDecoder =
    D.list
        (D.map2 Chapter
            (D.field "name" D.string)
            --name
            (D.field "content" D.string
                |> D.map
                    (String.words
                        >> List.map (\str -> str ++ "␣")
                        >> Array.fromList
                    )
            )
         --content
        )
        |> D.map Array.fromList


move : Book -> Book
move book =
    let
        bm =
            book.bookmark
    in
    case Array.get book.bookmark.chapter book.chapters of
        Just chapter ->
            case Array.get book.bookmark.word chapter.content of
                Just word ->
                    if book.bookmark.letter < String.length word - 1 then
                        { book | bookmark = { bm | letter = bm.letter + 1 } }

                    else
                        { book | bookmark = { bm | word = bm.word + 1, letter = 0 } }

                Nothing ->
                    book

        Nothing ->
            book


author : Book -> String
author book =
    book.author


name : Book -> String
name book =
    book.name


chapterName : Book -> String
chapterName book =
    .name (currChap book)


get : Book -> ( String, Maybe Char, String )
get book =
    let
        bm =
            book.bookmark

        currentChapter =
            currChap book

        currentWord =
            Maybe.withDefault "" <| Array.get bm.word currentChapter.content

        _ =
            Debug.log "words" (Array.slice (max 0 (bm.word - 1)) (bm.word + 4) currentChapter.content)
    in
    ( (Maybe.withDefault "" <| Array.get (bm.word - 1) currentChapter.content)
        ++ String.left bm.letter currentWord
    , currentWord
        |> String.toList
        |> Array.fromList
        |> Array.get bm.letter
    , String.slice (bm.letter + 1) (String.length currentWord) currentWord
        ++ (Array.slice (bm.word + 1) (bm.word + 3) currentChapter.content
                |> Array.toList
                |> String.join ""
           )
        ++ ((Maybe.withDefault "" <| Array.get (bm.word + 3) currentChapter.content)
                |> String.dropRight 1
           )
    )


curr : Bookmark -> Array String -> Maybe Char
curr bm txt =
    Maybe.withDefault "" (Array.get bm.word txt)
        |> String.toList
        |> Array.fromList
        |> Array.get bm.letter


currChap : Book -> Chapter
currChap book =
    Array.get book.bookmark.chapter book.chapters
        |> Maybe.withDefault { name = "error", content = Array.empty }


checkChar : Char -> Book -> ( Book, Bool )
checkChar char book =
    let
        currentChapter =
            currChap book

        current =
            curr book.bookmark currentChapter.content
    in
    case current of
        Just cr ->
            if (cr == char) || (char == ' ' && cr == '␣') then
                ( move book, False )

            else
                ( book, True )

        Nothing ->
            ( book, False )
