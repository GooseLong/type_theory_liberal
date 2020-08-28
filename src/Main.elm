module Main exposing (..)

import Book exposing (Book, bookDecoder, checkChar, get, move)
import Browser
import Browser.Events exposing (onKeyPress)
import Element as El
    exposing
        ( Element
        , column
        , el
        , htmlAttribute
        , layout
        , none
        , paragraph
        , row
        , text
        )
import Element.Background
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Process
import String
import Task exposing (succeed)



-- MODEL


type LoadState a
    = Failure Http.Error
    | Success a
    | Loading


loadMap : (a -> a) -> LoadState a -> LoadState a
loadMap f ls =
    case ls of
        Success x ->
            Success (f x)

        Failure err ->
            Failure err

        Loading ->
            Loading


type alias Game =
    { book : Book
    , options : List String
    , mistyped : Bool
    }


type alias Model =
    LoadState Game


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "assets/texts.json"
        , expect = Http.expectJson GotText textsDecoder
        }
    )



-- UPDATE


textsDecoder : D.Decoder Game
textsDecoder =
    D.map3 Game
        -- book
        (D.at [ "texts", "conquest_of_bread" ] bookDecoder)
        --options
        (D.field "names" (D.list D.string))
        --mistyped
        (D.succeed False)


type Msg
    = GotText (Result Http.Error Game)
    | Keyboard Key
    | MistypeTimeout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotText result, Loading ) ->
            case result of
                Ok game ->
                    ( Success game, Cmd.none )

                Err err ->
                    ( Failure err, Cmd.none )

        ( Keyboard key, Success game ) ->
            case key of
                Character char ->
                    let
                        ( newBook, error ) =
                            checkChar char game.book
                    in
                    if error then
                        ( Success { game | mistyped = True }, delay 15 MistypeTimeout )

                    else
                        ( Success { game | book = newBook }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( MistypeTimeout, Success game ) ->
            ( Success { game | mistyped = False }, Cmd.none )

        _ ->
            let
                _ =
                    Debug.log "oh no"
            in
            ( model, Cmd.none )


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Success _ ->
            onKeyPress keyDecoder

        _ ->
            Sub.none


type Key
    = Character Char
    | Control String


keyDecoder : D.Decoder Msg
keyDecoder =
    D.map (toKey >> Keyboard) <| D.field "key" D.string


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        case model of
            Loading ->
                text "Loading. . ."

            Failure err ->
                text <| Debug.toString err

            Success game ->
                let
                    book =
                        game.book

                    ( past_word, maybe_current_letter, future_words ) =
                        get book
                in
                column
                    [ El.width El.fill
                    , El.height El.fill
                    , Font.family
                        [ Font.typeface "Georgia"
                        , Font.serif
                        ]
                    , El.spacing 8
                    , El.padding 4
                    ]
                    [ column
                        [ El.width El.fill
                        , El.height (El.px 360)
                        , El.alignTop
                        ]
                        [ row
                            [ El.alignBottom
                            , El.centerX
                            , Font.size 44
                            , Font.family [ Font.monospace ]
                            , El.width El.shrink
                            ]
                            [ el pastStyle <| text past_word
                            , case maybe_current_letter of
                                Nothing ->
                                    text "[error]"

                                Just current_letter ->
                                    el (currentStyle game.mistyped) <|
                                        text (String.fromChar current_letter)
                            , el futureStyle <| text future_words
                            ]
                        ]
                    , el
                        [ El.centerX
                        , Font.size 20
                        , Font.extraLight
                        , El.spacing 3
                        , Font.color (El.rgb 0.4 0.4 0.4)
                        ]
                      <|
                        el [] <|
                            text (Book.chapterName book)
                    , el
                        [ El.centerX
                        , Font.size 22
                        ]
                      <|
                        text (Book.name book)
                    , el
                        [ El.centerX
                        , Font.size 18
                        , Font.italic
                        ]
                      <|
                        text (Book.author book)
                    ]


pastStyle : List (El.Attribute msg)
pastStyle =
    [ El.alignRight ]


currentStyle : Bool -> List (El.Attribute msg)
currentStyle mistyped =
    [ El.centerX
    , El.rgb 1 1 1 |> Font.color
    , Element.Background.color <|
        if mistyped then
            El.rgb 1 0 0

        else
            El.rgb 0.1 0.1 0.1
    , Font.shadow
        { offset = ( 0.2, 0.2 )
        , blur = 0.1
        , color = El.rgb 0.5 0.5 0.5
        }
    ]


futureStyle : List (El.Attribute msg)
futureStyle =
    [ El.alignLeft, El.rgb 0.7 0.7 0.7 |> Font.color ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
