module Main exposing (main)

-- https://github.com/nakagami/elm-examples/tree/master/reversi

import Browser
import Html exposing (Attribute, Html, caption, div, table, tbody, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Tuple
import Array
import Array2D

main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


-- MODEL
type alias Position = ( Int, Int )
type alias Disk = Int
type alias Cell = { position : Position, disk : Disk }

type alias Row = Maybe (Array.Array Cell)
type alias Board = Array2D.Array2D Cell

type Player
    = PlayerBlack
    | PlayerWhite

type GameState
    = Active
    | Tie
    | Won Player

type alias Model =
    { board : Board
    , currentPlayer : Player
    , gameState : GameState
    }


-- INIT
init : Model
init =
    { board = Array2D.indexedMap (\r c v -> Cell(r, c) v) (Array2D.repeat 8 8 0)
    , currentPlayer = PlayerBlack
    , gameState = Active
    }


-- VIEW
view : Model -> Html Position
view model =
    div []
        [ Html.node "style" [] [ text css ]
        , table []
            [ caption [] [ text "Reversi" ]
            , tbody [] (htmlFrom model.board)
            , tr []
                [ td [ colspan 8 ]
                    [ text <| stateStr model ]
                ]
            ]
        ]

htmlFrom : Board -> List (Html Position)
htmlFrom board =
    List.range 0 7
        |> List.map (\r -> filterByRow r board)
        |> List.map makeRowHtml

filterByRow : Int -> Board -> Row
filterByRow pos board =
    Array2D.getRow pos board

makeRowHtml : Row -> Html Position
makeRowHtml row =
    case row of
        Just rowArray ->
            tr [] (List.map makeCellHtml (Array.toList rowArray))
        Nothing ->
            tr [] []

makeCellHtml : Cell -> Html Position
makeCellHtml cell =
    td (cellAttributes cell) (cellTxt cell)


cellAttributes : Cell -> List (Attribute Position)
cellAttributes cell =
    [ onClick cell.position ]


cellTxt : Cell -> List (Html Position)
cellTxt cell =
    [ text <| diskToStr cell.disk ]


stateStr : Model -> String
stateStr model =
    case model.gameState of
        Active ->
            playerToStr model.currentPlayer ++ "'s turn."

        Tie ->
            "It's a tie :("

        Won winningPlayer ->
            playerToStr winningPlayer ++ " wins !!"


diskToStr : Disk -> String
diskToStr disk =
    case disk of
        0 ->
            " "
        1 ->
            "●"
        _ ->
            "○"

playerToStr : Player -> String
playerToStr p =
    case p of
        PlayerBlack ->
            "Player ●"
        PlayerWhite ->
            "Player ○"

playerToDisk : Player -> Disk
playerToDisk p =
    case p of
        PlayerBlack ->
            1
        PlayerWhite ->
            -1

css : String
css =
    """
    table { border-spacing:0px; }
    td {
        border: 1px solid black;
        width: 50px;
        height: 50px;
        background-color: green;
        text-align: center;
        vertical-align: middle;
        table-layout: fixed;
        padding: 1px 1px 1px 1px;
        font-size: 33px;
        text-align: center;
    }
    .Center {
        border-left: .1em solid black;
        border-right: .1em solid black;
    }
    .Middle {
        border-top: .1em solid black;
        border-bottom: .1em solid black;
    }
    """


-- UPDATE
update : Position -> Model -> Model
update clkPos model =
    let
        updatedBoard =
            updateCell clkPos model
    in
    if (canPlace clkPos model.board)  && model.gameState == Active then
        { model
            | board = updatedBoard
            , currentPlayer = updatePlayer model.currentPlayer
            , gameState = updateState updatedBoard
        }
    else
        model

-- TODO: check other condistions
canPlace : Position -> Board -> Bool
canPlace pos board =
    case Array2D.get (Tuple.first pos) (Tuple.second pos) board of
        Just cell ->
            cell.disk == 0
        Nothing ->
            False

-- TODO: reverse cells
updateCell : Position -> Model -> Board
updateCell pos model =
    case Array2D.get (Tuple.first pos) (Tuple.second pos) model.board of
        Just cell ->
            Array2D.set (Tuple.first pos) (Tuple.second pos) (Cell pos (playerToDisk model.currentPlayer)) model.board
        Nothing ->
            model.board

updateState : Board -> GameState
updateState board =
    let
        -- TODO: Game over?
        posWins = []
    in
        Active

markForPlayer : Player -> Disk
markForPlayer player =
    case player of
        PlayerBlack -> 1
        PlayerWhite -> -1

updatePlayer : Player -> Player
updatePlayer player =
    case player of
        PlayerBlack ->
            PlayerWhite

        PlayerWhite ->
            PlayerBlack
