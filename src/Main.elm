module Main exposing (..)

import Book exposing (Book, checkChar, get, move, newBook)
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


type alias Model =
    { book : LoadState Book
    , mistyped : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { book = Loading, mistyped = False }
    , Http.get
        { url = "assets/bread.txt"
        , expect = Http.expectString GotText
        }
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | Keyboard Key
    | MistypeTimeout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | book = Success <| newBook fullText }, Cmd.none )

                Err err ->
                    ( { model | book = Failure err }, Cmd.none )

        Keyboard key ->
            case key of
                Character char ->
                    case model.book of
                        Success oldBook ->
                            let
                                ( newBook, error ) =
                                    checkChar char oldBook
                            in
                            if error then
                                ( { model | mistyped = True }, delay 15 MistypeTimeout )

                            else
                                ( { model | book = loadMap (\_ -> newBook) model.book }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MistypeTimeout ->
            ( { model | mistyped = False }, Cmd.none )


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.book of
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
        case model.book of
            Loading ->
                text "Loading. . ."

            Failure err ->
                text <| Debug.toString err

            Success book ->
                let
                    ( past_word, maybe_current_letter, future_words ) =
                        get book
                in
                column
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
                                el (currentStyle model.mistyped) <|
                                    text (String.fromChar current_letter)
                        , el futureStyle <| text future_words
                        ]
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
