module ReversiTest exposing (..)

import Array2D
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


testModel1 : Model
testModel1 =
    { board = initBoard
    , currentPlayer = PlayerBlack
    , gameState = Active
    }


testBoard2 =
    [ [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, -1, 0, 0, 0, 0 ]
    , [ 0, 0, 0, -1, -1, 0, 0, 0 ]
    , [ 0, 0, 1, 1, 1, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    ]


testModel2 : Model
testModel2 =
    { board = Array2D.fromList testBoard2
    , currentPlayer = PlayerBlack
    , gameState = Active
    }


testBoard3 =
    [ [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, -1, 0, 0, 0, 0 ]
    , [ 0, 0, 0, -1, -1, 0, 0, 0 ]
    , [ 0, 0, 0, -1, 1, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
    ]


testModel3 : Model
testModel3 =
    { board = Array2D.fromList testBoard3
    , currentPlayer = PlayerWhite
    , gameState = Active
    }


blackDisk : Disk
blackDisk =
    playerToDisk PlayerBlack


suite : Test
suite =
    describe "Reversi"
        [ test "canPlacePos1 " <|
            \_ ->
                canPlacePos testModel1 blackDisk ( 4, 5 )
                    |> Expect.equal True
        , test "canPlacePos2 " <|
            \_ ->
                canPlacePos testModel1 blackDisk ( 5, 3 )
                    |> Expect.equal False
        , test "canPlacePos3 " <|
            \_ ->
                canPlacePos testModel2 blackDisk ( 4, 1 )
                    |> Expect.equal False
        , test "findBestPos " <|
            \_ ->
                findBestPos testModel3
                    |> Expect.equal ( 2, 2 )
        ]
