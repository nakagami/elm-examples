module Main exposing (main)

-- https://qiita.com/pxfnc/items/d2ea1ac004aebe1e222f

import Array exposing (Array)
import Browser
import Html exposing (Html, div, h1, pre, text)
import Html.Attributes exposing (style)
import List.Extra
import Random exposing (Generator)
import Random.Extra
import Random.List


type Cell
    = Free
    | Cell Int


type Row
    = Row (Array Cell)


type BingoCard
    = BingoCard (Array Row)


type alias Model =
    Maybe BingoCard


showBingoCard : BingoCard -> String
showBingoCard (BingoCard rows) =
    let
        -- セルを文字列にする
        showCell : Cell -> String
        showCell cell =
            case cell of
                Free ->
                    ""

                Cell n ->
                    String.fromInt n

        -- 行を文字列のリストに変換する
        rowToStrList : Row -> List String
        rowToStrList (Row cells) =
            cells
                |> Array.toList
                |> List.map showCell

        -- 表もヘッダも同じフォーマッタを使いたいので共通化
        formatRow : List String -> String
        formatRow list =
            list
                |> List.map (String.pad 4 ' ')
                |> String.join "|"

        -- ヘッダの定義
        header : List String
        header =
            [ "B", "I", "N", "G", "O" ]
    in
    rows
        |> Array.toList
        |> List.map rowToStrList
        |> (::) header
        |> List.map formatRow
        |> String.join "\n"


genBingoCard : Generator BingoCard
genBingoCard =
    let
        genColumn : Int -> Int -> Generator (List Cell)
        genColumn min_ max_ =
            List.range min_ max_
                |> List.map Cell
                |> Random.List.shuffle
                |> Random.map (List.take 5)

        mapToBingoCard : List (List Cell) -> BingoCard
        mapToBingoCard =
            List.map (Array.fromList >> Row) >> Array.fromList >> BingoCard
    in
    Random.Extra.sequence
        [ genColumn 1 15
        , genColumn 16 30
        , genColumn 31 45 |> Random.map (List.Extra.setAt 2 Free)
        , genColumn 46 60
        , genColumn 61 75
        ]
        |> Random.map List.Extra.transpose
        |> Random.map mapToBingoCard


type Msg
    = GenerateBingo BingoCard


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing
    , Random.generate GenerateBingo genBingoCard
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateBingo card ->
            ( Just card, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        formattedBingoCard : String
        formattedBingoCard =
            model
                |> Maybe.map showBingoCard
                |> Maybe.withDefault "creating..."
    in
    div []
        [ h1 [] [ text "Elm Bingo" ]
        , pre [ style "font-family" "Courier" ] [ text formattedBingoCard ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
