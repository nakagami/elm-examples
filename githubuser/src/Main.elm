module Main exposing (..)

-- https://github.com/nakagami/elm-examples/tree/master/githubuser

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias User =
    { id : Int
    , login : String
    , name : String
    }


userDecoder : Decoder User
userDecoder =
    Json.Decode.map3 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "login" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)


type Model
    = Failure
    | Loading
    | Success User


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getGithubUser )



-- UPDATE


type Msg
    = MorePlease
    | GotUser (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getGithubUser )

        GotUser result ->
            case result of
                Ok user ->
                    ( Success user, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Github User" ]
        , viewUser model
        ]


viewUser : Model -> Html Msg
viewUser model =
    case model of
        Failure ->
            div []
                [ text "I could not load . "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success user ->
            div []
                [ button [ onClick MorePlease, style "display" "block" ] [ text "Get user info !" ]
                , ul []
                    [ li [] [ text user.login ]
                    , li [] [ text user.name ]
                    ]
                ]



-- HTTP


getGithubUser : Cmd Msg
getGithubUser =
    Http.get
        { url = "https://api.github.com/users/nakagami"
        , expect = Http.expectJson GotUser userDecoder
        }
