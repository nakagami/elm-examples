module Main exposing (main)

-- https://github.com/nakagami/elm-examples/tree/master/reversi

import Array
import Array2D
import Browser
import Html exposing (Attribute, Html, caption, div, table, tbody, td, text, tr)
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


type alias Position =
    ( Int, Int )



-- black: -1 white: 1 empty: 0


type alias Disk =
    Int


type alias PosDisk =
    { position : Position, disk : Disk }


type alias Row =
    Maybe (Array.Array PosDisk)


type alias Board =
    Array2D.Array2D Disk


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


type alias Msg =
    Position



-- INIT


init : Model
init =
    { board = Array2D.fromList initBoard
    , currentPlayer = PlayerBlack
    , gameState = Active
    }


initBoard =
    [ [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 1, -1, 0, 0, 0 ]
    , [ 0, 0, 0, -1, 1, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    ]

allDirections = [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 1, -1 ), ( 0, -1 ) ]


-- VIEW


view : Model -> Html Msg
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
    Array2D.getRow pos (Array2D.indexedMap (\r c v -> PosDisk ( r, c ) v) board)


makeRowHtml : Row -> Html Position
makeRowHtml row =
    case row of
        Just rowArray ->
            tr [] (List.map makeCellHtml (Array.toList rowArray))

        Nothing ->
            tr [] []


makeCellHtml : PosDisk -> Html Position
makeCellHtml cell =
    td (cellAttributes cell) (cellTxt cell)


cellAttributes : PosDisk -> List (Attribute Position)
cellAttributes cell =
    [ onClick cell.position ]


cellTxt : PosDisk -> List (Html Position)
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
            -- whiteDisk
            "○"

        _ ->
            "●"


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
            -1

        PlayerWhite ->
            1



-- whiteDisk


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
    if canPlace model clkPos && model.gameState == Active then
        { model
            | board = updatedBoard
            , currentPlayer = updatePlayer model.currentPlayer
            , gameState = updateState updatedBoard
        }

    else
        model



-- There is my disk in the direction of (deltaX, deltaY) from pos


hasMyDisk : Model -> Position -> ( Int, Int ) -> Bool
hasMyDisk model pos ( deltaX, deltaY ) =
    let
        nextX =
            Tuple.first pos + deltaX

        nextY =
            Tuple.second pos + deltaY
    in
    case Array2D.get nextX nextY model.board of
        Just disk ->
            if disk == 0 then
                False

            else if disk == playerToDisk model.currentPlayer then
                True

            else
                hasMyDisk model ( nextX, nextY ) ( deltaX, deltaY )

        Nothing ->
            False


canPlaceDirection : Model -> Position -> ( Int, Int ) -> Bool
canPlaceDirection model pos ( deltaX, deltaY ) =
    let
        nextX =
            Tuple.first pos + deltaX

        nextY =
            Tuple.second pos + deltaY
    in
    case Array2D.get nextX nextY model.board of
        Just disk ->
            if disk == 0 || disk == playerToDisk model.currentPlayer then
                False

            else
                hasMyDisk model ( nextX, nextY ) ( deltaX, deltaY )

        Nothing ->
            False



-- TODO: check other condistions


canPlace : Model -> Position -> Bool
canPlace model pos =
    let
        isEmpty =
            case Array2D.get (Tuple.first pos) (Tuple.second pos) model.board of
                Just disk ->
                    disk == 0

                Nothing ->
                    False
    in
    if isEmpty then
        List.any
            (\r -> r==True)
            (List.map (canPlaceDirection model pos) allDirections)
    else
        False



-- reverse disks


reverseDiskDirection : Disk -> Position -> ( Int, Int ) Board -> Board
reverseDiskDirection myDisk pos ( deltaX, deltaY ) board =
    let
        nextX =
            Tuple.first pos + deltaX

        nextY =
            Tuple.second pos + deltaY
    in
    case Array2D.get nextX nextY board of
        Just disk ->
            if myDisk == disk * -1 then
                reverseDiskDirection myDisk (nextX, nextY) board (deltaX, deltaY)
            else
                board

        Nothing ->
            board

reverseDisk: Board -> Disk -> Position -> Board
reverseDisk board myDisk pos =
    List.foldl (reverseDiskDirection board myDisk pos) allDirections


updateCell : Position -> Model -> Board
updateCell pos model =
    case Array2D.get (Tuple.first pos) (Tuple.second pos) model.board of
        Just disk ->
            Array2D.set (Tuple.first pos) (Tuple.second pos) (playerToDisk model.currentPlayer) model.board

        Nothing ->
            model.board


updateState : Board -> GameState
updateState board =
    let
        -- TODO: Game over?
        posWins =
            []
    in
    Active



updatePlayer : Player -> Player
updatePlayer player =
    case player of
        PlayerBlack ->
            PlayerWhite

        PlayerWhite ->
            PlayerBlack
