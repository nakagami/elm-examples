module Main exposing (..)

-- https://github.com/nakagami/elm-examples/tree/master/reversi_npc

import Array
import Array2D
import Browser
import Dict
import Html exposing (Attribute, Html, caption, div, table, tbody, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Tuple


main =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }



-- MODEL


type alias Position =
    ( Int, Int )


type alias Disk =
    -- black: -1 white: 1 empty: 0
    Int


type alias PosDisk =
    { pos : Position, disk : Disk }


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



-- NPC


npcMatrix =
    Array2D.fromList
        [ [ 30, -12, 0, -1, -1, 0, -12, 30 ]
        , [ -12, -15, -3, -3, -3, -3, -15, -12 ]
        , [ 0, -3, 0, -1, -1, 0, -3, 0 ]
        , [ -1, -3, -1, -1, -1, -1, -3, -1 ]
        , [ -1, -3, -1, -1, -1, -1, -3, -1 ]
        , [ 0, -3, 0, -1, -1, 0, -3, 0 ]
        , [ -12, -15, -3, -3, -3, -3, -15, -12 ]
        , [ 30, -12, 0, -1, -1, 0, -12, 30 ]
        ]


getWeight : Position -> Int
getWeight ( posX, posY ) =
    case Array2D.get posX posY npcMatrix of
        Just weight ->
            weight

        Nothing ->
            0


calcScore : Model -> Position -> ( Int, Position )
calcScore model pos =
    let
        updatedModel =
            updateCell pos model

        myDisk =
            playerToDisk model.currentPlayer

        myScore =
            allPositions
                |> List.filter (chkDisk model myDisk)
                |> List.map getWeight
                |> List.sum

        opponentScore =
            allPositions
                |> List.filter (chkDisk model (myDisk * -1))
                |> List.map getWeight
                |> List.sum
    in
    ( myScore - opponentScore, pos )


findBestPos : Model -> Position
findBestPos model =
    let
        myDisk =
            playerToDisk model.currentPlayer

        scorePosList =
            allPositions
                |> List.filter (canPlacePos model myDisk)
                |> List.map (calcScore model)

        maxScore =
            scorePosList
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault -9999999
    in
    Dict.fromList scorePosList
        |> Dict.get maxScore
        |> Maybe.withDefault ( -1, -1 )



-- INIT


initModel : Model
initModel =
    { board = initBoard
    , currentPlayer = PlayerBlack
    , gameState = Active
    }


initBoard =
    Array2D.fromList
        [ [ 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 1, -1, 0, 0, 0 ]
        , [ 0, 0, 0, -1, 1, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
        ]


allPositions : List Position
allPositions =
    List.concatMap (\r -> List.map (\c -> ( r, c )) (List.range 0 7)) (List.range 0 7)


allDirections : List Position
allDirections =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 1, -1 ), ( 0, -1 ) ]



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


filterByRow : Int -> Board -> List PosDisk
filterByRow pos board =
    case Array2D.getRow pos (Array2D.indexedMap (\r c v -> PosDisk ( r, c ) v) board) of
        Just rowArray ->
            Array.toList rowArray

        Nothing ->
            []


makeRowHtml : List PosDisk -> Html Msg
makeRowHtml row =
    tr [] (List.map makeCellHtml row)


makeCellHtml : PosDisk -> Html Msg
makeCellHtml cell =
    td [ onClick cell.pos ] [ text <| diskToStr cell.disk ]


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
        updatedModel =
            updateCell clkPos model
    in
    if canPlacePos model (playerToDisk model.currentPlayer) clkPos && model.gameState == Active then
        { updatedModel
            | currentPlayer = updatePlayer model updatedModel.currentPlayer
            , gameState = updateState updatedModel
        }

    else
        model



-- There is my disk in the direction of (deltaX, deltaY) from pos


hasSameDisk : Model -> Disk -> Position -> ( Int, Int ) -> Bool
hasSameDisk model disk ( posX, posY ) ( deltaX, deltaY ) =
    let
        nextX =
            posX + deltaX

        nextY =
            posY + deltaY
    in
    case Array2D.get nextX nextY model.board of
        Just nextPosDisk ->
            if nextPosDisk == 0 then
                False

            else if nextPosDisk == disk then
                True

            else
                hasSameDisk model disk ( nextX, nextY ) ( deltaX, deltaY )

        Nothing ->
            False


canPlaceDirection : Model -> Disk -> Position -> ( Int, Int ) -> Bool
canPlaceDirection model disk ( posX, posY ) ( deltaX, deltaY ) =
    let
        nextX =
            posX + deltaX

        nextY =
            posY + deltaY
    in
    case Array2D.get nextX nextY model.board of
        Just nextPosDisk ->
            if nextPosDisk == 0 || nextPosDisk == disk then
                False

            else
                hasSameDisk model disk ( nextX, nextY ) ( deltaX, deltaY )

        Nothing ->
            False


chkDisk : Model -> Disk -> Position -> Bool
chkDisk model disk ( posX, posY ) =
    case Array2D.get posX posY model.board of
        Just posDisk ->
            posDisk == disk

        Nothing ->
            False


canPlacePos : Model -> Disk -> Position -> Bool
canPlacePos model disk ( posX, posY ) =
    if chkDisk model 0 ( posX, posY ) then
        List.length
            (List.filter
                (canPlaceDirection model disk ( posX, posY ))
                allDirections
            )
            > 0

    else
        False



-- reverse disks


reverseDiskDirection : Board -> Disk -> Position -> ( Int, Int ) -> Board
reverseDiskDirection board myDisk ( posX, posY ) ( deltaX, deltaY ) =
    let
        nextX =
            posX + deltaX

        nextY =
            posY + deltaY

        canReverse =
            case Array2D.get nextX nextY board of
                Just disk ->
                    disk == myDisk * -1

                Nothing ->
                    False
    in
    if canReverse then
        reverseDiskDirection
            (Array2D.set nextX nextY myDisk board)
            myDisk
            ( nextX, nextY )
            ( deltaX, deltaY )

    else
        board


reverseDiskDirections : Board -> Disk -> Position -> List ( Int, Int ) -> Board
reverseDiskDirections board myDisk pos directions =
    let
        updatedBoard =
            case List.head directions of
                Just delta ->
                    reverseDiskDirection board myDisk pos delta

                Nothing ->
                    board
    in
    case List.tail directions of
        Just restDirections ->
            reverseDiskDirections updatedBoard myDisk pos restDirections

        Nothing ->
            updatedBoard


reverseDisk : Position -> Model -> Model
reverseDisk pos model =
    let
        myDisk =
            playerToDisk model.currentPlayer

        updatedBoard =
            reverseDiskDirections model.board
                myDisk
                pos
                (List.filter
                    (canPlaceDirection model myDisk pos)
                    allDirections
                )
    in
    { model | board = updatedBoard }


updateCell : Position -> Model -> Model
updateCell ( posX, posY ) model =
    let
        model2 =
            case Array2D.get posX posY model.board of
                Just disk ->
                    { model
                        | board =
                            Array2D.set posX posY (playerToDisk model.currentPlayer) model.board
                    }

                Nothing ->
                    model
    in
    reverseDisk ( posX, posY ) model2


updateState : Model -> GameState
updateState model =
    let
        hasEmpty =
            List.length (List.filter (chkDisk model 0) allPositions) > 0

        blackCount =
            List.length (List.filter (chkDisk model -1) allPositions)

        whiteCount =
            List.length (List.filter (chkDisk model 1) allPositions)
    in
    if hasEmpty then
        Active

    else if blackCount > whiteCount then
        Won PlayerBlack

    else if whiteCount > blackCount then
        Won PlayerWhite

    else
        Tie


updatePlayer : Model -> Player -> Player
updatePlayer model player =
    let
        myDisk =
            playerToDisk model.currentPlayer
    in
    if
        List.length
            (List.filter
                (canPlacePos model (myDisk * -1))
                allPositions
            )
            > 0
    then
        case player of
            PlayerBlack ->
                PlayerWhite

            PlayerWhite ->
                PlayerBlack

    else
        player
