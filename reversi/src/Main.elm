module Main exposing (main)

-- https://github.com/nakagami/elm-examples/tree/master/reversi

import Browser
import Html exposing (Attribute, Html, caption, div, table, tbody, tr, td, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Tuple


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL

blackCell = 1
whiteCell = -1
emptyCell = 0

type alias Position =
    ( Int, Int )


type alias Value =
    Int

type alias Cell =
    { position : Position, value : Int }


type alias Row =
    List Cell


type alias Board =
    List Cell


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


rows : List Int
rows =
    List.range 0 7


columns : List Int
columns =
    List.range 0 7


allPositions : List Position
allPositions =
    columns
        |> List.map (\r -> List.map (\c -> ( c, r )) rows)
        |> List.concat


init : Model
init =
    { board = List.map (\p -> Cell p emptyCell) allPositions
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
    rows
        |> List.map (\r -> filterByRow r board)
        |> List.map makeRowHtml


filterByRow : Int -> Board -> Row
filterByRow pos board =
    List.filter (\cell -> Tuple.first cell.position == pos) board


filterByCol : Int -> Board -> Row
filterByCol pos board =
    List.filter (\cell -> Tuple.second cell.position == pos) board


makeRowHtml : Row -> Html Position
makeRowHtml row =
    tr [] (List.map makeCellHtml row)


makeCellHtml : Cell -> Html Position
makeCellHtml cell =
    td (cellAttributes cell) (cellTxt cell)


cellAttributes : Cell -> List (Attribute Position)
cellAttributes cell =
    let
        ( v, h ) =
            cell.position
    in
    [ onClick cell.position ]


cellTxt : Cell -> List (Html Position)
cellTxt cell =
    [ text <| valToStr cell.value ]


stateStr : Model -> String
stateStr model =
    case model.gameState of
        Active ->
            playerToStr model.currentPlayer ++ "'s turn."

        Tie ->
            "It's a tie :("

        Won winningPlayer ->
            playerToStr winningPlayer ++ " wins !!"


valToStr : Value -> String
valToStr val =
    case val of
        0 ->    -- emptyCell
            " "
        1 ->    -- blackCell
            "●"
        _ ->    -- whiteCell(-1)
            "○"


playerToStr : Player -> String
playerToStr p =
    case p of
        PlayerBlack ->
            "Player ●"

        PlayerWhite ->
            "Player ○"


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

        clkPosIsEmpty =
            model.board |> List.member { position = clkPos, value = emptyCell }
    in
    if clkPosIsEmpty && model.gameState == Active then
        { model
            | board = updatedBoard
            , currentPlayer = updatePlayer model.currentPlayer
            , gameState = updateState updatedBoard
        }

    else
        model


-- TODO: reverse cells
updateCell : Position -> Model -> Board
updateCell clkPos model =
    List.map
        (\cell ->
            if cell.position == clkPos then
                { position = cell.position
                , value = markForPlayer model.currentPlayer
                }

            else
                cell
        )
        model.board


updateState : Board -> GameState
updateState board =
    let
        hLines =
            rows
                |> List.map (\row -> filterByRow row board)

        vLines =
            columns
                |> List.map (\col -> filterByCol col board)

--      TODO: Check game over
--        diagonals =
--            [ List.filter (\c -> c.position == ( Top, Right ) || c.position == ( Middle, Center ) || c.position == ( Bottom, Left )) board
--            , List.filter (\c -> c.position == ( Bottom, Right ) || c.position == ( Middle, Center ) || c.position == ( Top, Left )) board
--            ]
        diagonals =
              []

        posWins =
            List.concat [ hLines, vLines, diagonals ]
    in
    if List.any (\line -> isAllBlack line) posWins then
        Won PlayerBlack

    else if List.any (\line -> isAllWhite line) posWins then
        Won PlayerWhite

    else if List.any (\line -> hasEmpty line) posWins then
        Active

    else
        Tie


isAllBlack : List Cell -> Bool
isAllBlack line =
    List.all (\c -> c.value == blackCell) line


isAllWhite : List Cell -> Bool
isAllWhite line =
    List.all (\c -> c.value == whiteCell) line


hasEmpty : List Cell -> Bool
hasEmpty line =
    List.any (\c -> c.value == emptyCell) line


markForPlayer : Player -> Value
markForPlayer player =
    case player of
        PlayerBlack ->
            blackCell

        PlayerWhite ->
            whiteCell


updatePlayer : Player -> Player
updatePlayer player =
    case player of
        PlayerBlack ->
            PlayerWhite

        PlayerWhite ->
            PlayerBlack
