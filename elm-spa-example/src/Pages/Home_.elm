module Pages.Home_ exposing (Model, Msg, page)

import Gen.Params.Home_ exposing (Params)
import Html exposing (text)
import Html.Events exposing (onClick)
import Page
import Request
import Shared
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { counter : Int
    }


init : Model
init =
    { counter = 0
    }



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + 1 }

        Decrement ->
            { model | counter = model.counter - 1 }



-- VIEW


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        UI.layout
            [ Html.h1 [] [ Html.text "Local storage" ]
            , Html.button [ Html.Events.onClick Increment ] [ Html.text "+" ]
            , Html.p [] [ Html.text ("Count: " ++ String.fromInt model.counter) ]
            , Html.button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
            ]
    }
