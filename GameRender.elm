module GameRender exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Array
import Char
import Svg exposing (..)
import Svg.Attributes exposing (..)


renderGame : Model -> Svg Msg
renderGame model =
    svg [ width "900", height "900" ] [ renderBoardYAxis model, renderBoardXAxis model, renderBoard model, renderPieces model ]


pieceImagePath : Piece -> String
pieceImagePath piece =
    let
        cColor =
            if piece.cColor == White then
                "white"
            else
                "black"
    in
        case piece.rank of
            King a ->
                "king_" ++ cColor

            Queen ->
                "queen_" ++ cColor

            Bishop ->
                "bishop_" ++ cColor

            Knight ->
                "knight_" ++ cColor

            Rook ->
                "rook_" ++ cColor

            Pawn ->
                "pawn_" ++ cColor

            _ ->
                "none"


renderPiece : Piece -> Svg Msg
renderPiece piece =
    let
        fullImagePath =
            "./pieces/" ++ (pieceImagePath piece) ++ ".svg"

        ( row, col ) =
            piece.pos
    in
        image [ 100 * col |> toString |> x, 700 - (100 * row) |> toString |> y, width "80", height "80", xlinkHref fullImagePath ] []


renderPieces : Model -> Svg Msg
renderPieces model =
    g [ transform "translate(100, 100)" ] (List.map renderPiece model.board.pieces)


renderBoard : Model -> Svg Msg
renderBoard model =
    let
        board =
            Array.initialize 64 (renderBoardSquare model)
    in
        g [ transform "translate(100,100)" ] (Array.toList board)


renderBoardXAxis : Model -> Svg Msg
renderBoardXAxis model =
    let
        numberToLetter : Int -> String
        numberToLetter num =
            Char.toCode 'a' |> (+) num |> Char.fromCode |> String.fromChar

        xAxisConstructor : Int -> Svg Msg
        xAxisConstructor index =
            g [ transform ("translate(" ++ (toString (100 * index)) ++ ",0)") ]
                [ rect [ width "100", height "100", Svg.Attributes.style "fill:white;" ] []
                , Svg.text_ [ x "50", y "50" ] [ Svg.text (numberToLetter index) ]
                ]

        xAxis =
            Array.initialize 8 xAxisConstructor
    in
        g [ transform "translate(100,0)" ] (Array.toList xAxis)


renderBoardYAxis : Model -> Svg Msg
renderBoardYAxis model =
    let
        yAxisConstructor : Int -> Svg Msg
        yAxisConstructor index =
            g [ transform ("translate(0," ++ (toString (100 * index)) ++ ")") ]
                [ rect [ width "100", height "100", Svg.Attributes.style "fill:white;" ] []
                , Svg.text_ [ x "50", y "50" ] [ Svg.text (toString (index + 1)) ]
                ]

        yAxis =
            Array.initialize 8 yAxisConstructor
    in
        g [ transform "translate(0, 100)" ] (Array.toList yAxis)


renderBoardSquare : Model -> Int -> Svg msg
renderBoardSquare model index =
    let
        col =
            index % 8

        row =
            index // 8

        squareColor =
            if (col + row) % 2 == 0 then
                "fill:rgb(0,0,255);"
            else
                "fill:rgb(255,0,0)"
    in
        rect [ x (toString (700 - 100 * col)), y (toString (700 - 100 * row)), width "100", height "100", Svg.Attributes.style squareColor ] []
