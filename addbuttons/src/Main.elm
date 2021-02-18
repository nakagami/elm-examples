module Main exposing (main)

-- https://ellie-app.com/3cDw7rNCjx6a1

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Array exposing (..)


type alias Model =
    { elements : Array Int }


initialModel : Model
initialModel =
    Model (fromList [])


type Msg
    = Increment Int
    | Add


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment index ->
            case get index model.elements of
                Just element ->
                    { model | elements = set index (element + 1) model.elements }

                Nothing ->
                    model

        Add ->
            { model | elements = push 0 model.elements }


view : Model -> Html Msg
view model =
    div []
        [ div [] (toList (Array.indexedMap viewElement model.elements))
        , button [ onClick Add ] [ text "Add" ]
        ]


viewElement : Int -> Int -> Html Msg
viewElement index element =
    div []
        [ text (String.fromInt element)
        , button [ onClick (Increment index) ] [ text "inc" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
