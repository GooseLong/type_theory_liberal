module Book exposing (..)

import Array exposing (Array)
import Char exposing (Char)


type Book
    = Book Bookmark (Array String)


type Bookmark
    = Bookmark { word : Int, letter : Int }


newBook : String -> Book
newBook txt =
    Book (Bookmark { word = 0, letter = 0 })
        (String.words txt
            |> List.map (\str -> str ++ "␣")
            |> Array.fromList
        )


move : Book -> Book
move ((Book (Bookmark bm) txt) as book) =
    case Array.get bm.word txt of
        Just word ->
            if bm.letter < String.length word - 1 then
                Book (Bookmark { bm | letter = bm.letter + 1 }) txt

            else
                Book (Bookmark { bm | word = bm.word + 1, letter = 0 }) txt

        Nothing ->
            book


get : Book -> ( String, Maybe Char, String )
get (Book (Bookmark bm) txt) =
    let
        currentWord =
            Maybe.withDefault "" <| Array.get bm.word txt

        _ =
            Debug.log "words" (Array.slice (max 0 (bm.word - 1)) (bm.word + 4) txt)
    in
    ( (Maybe.withDefault "" <| Array.get (bm.word - 1) txt)
        ++ String.left bm.letter currentWord
    , currentWord
        |> String.toList
        |> Array.fromList
        |> Array.get bm.letter
    , String.slice (bm.letter + 1) (String.length currentWord) currentWord
        ++ (Array.slice (bm.word + 1) (bm.word + 3) txt
                |> Array.toList
                |> String.join ""
           )
        ++ ((Maybe.withDefault "" <| Array.get (bm.word + 3) txt)
                |> String.dropRight 1
           )
    )


curr : Bookmark -> Array String -> Maybe Char
curr (Bookmark bm) txt =
    Maybe.withDefault "" (Array.get bm.word txt)
        |> String.toList
        |> Array.fromList
        |> Array.get bm.letter


checkChar : Char -> Book -> Book
checkChar char ((Book bm txt) as book) =
    let
        current =
            curr bm txt
    in
    case current of
        Just cr ->
            if (cr == char) || (char == ' ' && cr == '␣') then
                move book

            else
                book

        Nothing ->
            book
