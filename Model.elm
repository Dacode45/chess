module Model exposing (..)

import Util exposing (..)


type Rank
    = King Bool
      -- Bool indicates if king has moved this game
    | Queen
    | Knight
    | Bishop
    | Pawn
    | Rook Bool
      -- Bool indicates if Rook has moved this game
    | None


type CColor
    = White
    | Black


type alias Piece =
    { cColor : CColor
    , rank : Rank
    , pos : ( Int, Int )
    , lastState : List State
    }


type alias Board =
    { pieces : List Piece
    , whiteCaptured : List Piece
    , blackCaptured : List Piece
    }


type State
    = MoveTo ( Int, Int )
    | Capture Piece
    | CastleWith Piece
    | Promote
    | JumpAhead


validRank : Rank -> Bool
validRank rank =
    if rank == None then
        True
    else
        False


validPos : ( Int, Int ) -> Bool
validPos ( x, y ) =
    if (x < 0) || (x > 7) || (y < 0) || (y < 7) then
        False
    else
        True


emptyBoard : Board
emptyBoard =
    { pieces = []
    , whiteCaptured = []
    , blackCaptured = []
    }


newBoard : Board
newBoard =
    let
        whitePawns =
            List.map (\x -> Piece White Pawn ( 1, x ) []) (List.range 0 7)

        whiteSpecial =
            [ Piece White (Rook False) ( 0, 0 ) []
            , Piece White Knight ( 0, 1 ) []
            , Piece White Bishop ( 0, 2 ) []
            , Piece White (King False) ( 0, 3 ) []
            , Piece White Queen ( 0, 4 ) []
            , Piece White Bishop ( 0, 5 ) []
            , Piece White Knight ( 0, 6 ) []
            , Piece White (Rook False) ( 0, 7 ) []
            ]

        blackPawns =
            List.map (\x -> Piece Black Pawn ( 6, x ) []) (List.range 0 7)

        blackSpecial =
            [ Piece Black (Rook False) ( 7, 0 ) []
            , Piece Black Knight ( 7, 1 ) []
            , Piece Black Bishop ( 7, 2 ) []
            , Piece Black (King False) ( 7, 3 ) []
            , Piece Black Queen ( 7, 4 ) []
            , Piece Black Bishop ( 7, 5 ) []
            , Piece Black Knight ( 7, 6 ) []
            , Piece Black (Rook False) ( 7, 7 ) []
            ]
    in
        { pieces = whitePawns ++ whiteSpecial ++ blackPawns ++ blackSpecial
        , whiteCaptured = []
        , blackCaptured = []
        }


type alias Model =
    { board : Board
    , command : String
    }


extractPiece : List Piece -> Piece -> ( Maybe Piece, List Piece )
extractPiece pieces piece =
    let
        newList =
            List.filter (\p -> p.cColor /= piece.cColor || p.rank /= piece.rank || p.pos /= piece.pos) pieces

        newPiece =
            List.filter (\p -> p.cColor == piece.cColor && p.rank == piece.rank && p.pos == piece.pos) pieces
    in
        ( List.head newPiece, newList )


applyStates : Board -> Piece -> List State -> ( Piece, Board )
applyStates board piece states =
    let
        apply : Board -> Piece -> List State -> ( Piece, Board )
        apply board piece stateList =
            case List.head stateList of
                Just s ->
                    let
                        ( updatedPiece, updatedBoard ) =
                            applyState board piece s
                    in
                        apply updatedBoard updatedPiece (List.drop 1 stateList)

                _ ->
                    ( piece, board )

        ( updatedPiece, updatedBoard ) =
            apply board piece states

        ( newPiece, others ) =
            extractPiece updatedBoard.pieces updatedPiece
    in
        case newPiece of
            Just thing ->
                let
                    p =
                        { thing | lastState = states }
                in
                    ( p
                    , { updatedBoard | pieces = p :: others }
                    )

            _ ->
                ( { updatedPiece | lastState = states }, updatedBoard )


applyState : Board -> Piece -> State -> ( Piece, Board )
applyState board templatePiece state =
    let
        ( realPiece, otherPieces ) =
            extractPiece board.pieces templatePiece
    in
        case realPiece of
            Just piece ->
                case state of
                    MoveTo newPos ->
                        let
                            newPiece =
                                { piece | pos = newPos }
                        in
                            ( newPiece, { board | pieces = newPiece :: otherPieces } )

                    Capture p ->
                        let
                            ( toCapture, others ) =
                                extractPiece otherPieces p
                        in
                            case toCapture of
                                Just tC ->
                                    if piece.cColor == White then
                                        ( { piece | pos = tC.pos }
                                        , { board
                                            | pieces = { piece | pos = tC.pos } :: others
                                            , whiteCaptured = tC :: board.whiteCaptured
                                          }
                                        )
                                    else
                                        ( { piece | pos = tC.pos }
                                        , { board
                                            | pieces = { piece | pos = tC.pos } :: others
                                            , blackCaptured = tC :: board.blackCaptured
                                          }
                                        )

                                _ ->
                                    ( piece, board )

                    CastleWith castle ->
                        let
                            ( actualCastle, others ) =
                                extractPiece otherPieces castle
                        in
                            case actualCastle of
                                Just c ->
                                    let
                                        ( cR, cC ) =
                                            c.pos

                                        ( row, col ) =
                                            piece.pos
                                    in
                                        if cC < col then
                                            ( { piece | pos = ( row, col - 2 ) }, { board | pieces = [ { piece | pos = ( row, col - 2 ) } ] ++ [ { c | pos = ( row, col - 1 ) } ] ++ others } )
                                        else
                                            ( { piece | pos = ( row, col + 2 ) }, { board | pieces = [ { piece | pos = ( row, col + 2 ) } ] ++ [ { c | pos = ( row, col + 1 ) } ] ++ others } )

                                _ ->
                                    ( piece, board )

                    Promote ->
                        let
                            newPiece =
                                (Piece piece.cColor Queen piece.pos piece.lastState)
                        in
                            ( newPiece, { board | pieces = newPiece :: otherPieces } )

                    JumpAhead ->
                        ( piece, board )

            _ ->
                ( templatePiece, board )


pieceAt : List Piece -> ( Int, Int ) -> Bool
pieceAt pieces ( row, col ) =
    List.any (\piece -> piece.pos == ( row, col )) pieces


getPieceAt : List Piece -> ( Int, Int ) -> Maybe Piece
getPieceAt pieces ( row, col ) =
    pieces
        |> List.filter (\piece -> piece.pos == ( row, col ))
        |> List.head


inCheck : Board -> Piece -> Bool
inCheck board piece =
    case piece.rank of
        King moved ->
            let
                opposingPieces =
                    List.filter (\p -> p.cColor /= piece.cColor) board.pieces

                states =
                    (List.map (getSuccesorState board) opposingPieces)
                        |> List.foldr (++) []
                        |> List.foldr (++) []
            in
                if
                    List.any
                        (\state ->
                            case state of
                                Capture p ->
                                    if (p.rank == King True || p.rank == King False) && (p.cColor == piece.cColor) then
                                        True
                                    else
                                        False

                                _ ->
                                    False
                        )
                        states
                then
                    True
                else
                    False

        _ ->
            False


getSuccesorState : Board -> Piece -> List (List State)
getSuccesorState board piece =
    case piece.rank of
        King a ->
            getSuccesorStateKing board piece

        Queen ->
            getSuccesorStateQueen board piece

        Bishop ->
            getSuccessorStateBishop board piece

        Knight ->
            getSuccesorStateKnight board piece

        Rook a ->
            getSuccesorStateRook board piece

        Pawn ->
            getSuccesorStatePawn board piece

        None ->
            []


moveAndCaptureStates : Board -> Piece -> ( Int, Int ) -> List (List State)
moveAndCaptureStates board piece dir =
    let
        opposingPieces =
            List.filter (\p -> p.cColor /= piece.cColor) board.pieces

        friendlyPieces =
            List.filter (\p -> p.cColor == piece.cColor) board.pieces

        f : ( Int, Int ) -> ( Int, Int ) -> List (List State) -> List (List State)
        f ( r, c ) ( vert, hor ) runningList =
            let
                ( newR, newC ) =
                    ( r + vert, c + hor )
            in
                if validPos ( newR, newC ) then
                    if pieceAt opposingPieces ( newR, newC ) then
                        let
                            pieceToCapture =
                                Maybe.withDefault (Piece White None ( -1, -1 ) []) (getPieceAt opposingPieces ( newR, newC ))
                        in
                            [ Capture pieceToCapture ] :: f ( newR, newC ) ( vert, hor ) runningList
                    else if pieceAt friendlyPieces ( newR, newC ) then
                        runningList
                    else
                        [ MoveTo ( newR, newC ) ] :: f ( newR, newC ) ( vert, hor ) runningList
                else
                    runningList
    in
        f piece.pos dir []


getSuccesorStateRook : Board -> Piece -> List (List State)
getSuccesorStateRook board piece =
    let
        possibleStates =
            (moveAndCaptureStates board piece ( 1, 0 ))
                ++ (moveAndCaptureStates board piece ( -1, 0 ))
                ++ (moveAndCaptureStates board piece ( 0, 1 ))
                ++ (moveAndCaptureStates board piece ( 0, -1 ))
    in
        possibleStates


getSuccessorStateBishop : Board -> Piece -> List (List State)
getSuccessorStateBishop board piece =
    (moveAndCaptureStates board piece ( 1, 1 ))
        ++ (moveAndCaptureStates board piece ( 1, -1 ))
        ++ (moveAndCaptureStates board piece ( -1, 1 ))
        ++ (moveAndCaptureStates board piece ( -1, -1 ))


getSuccesorStateQueen : Board -> Piece -> List (List State)
getSuccesorStateQueen board piece =
    (getSuccesorStateRook board piece) ++ (getSuccessorStateBishop board piece)


getSuccesorStateKnight : Board -> Piece -> List (List State)
getSuccesorStateKnight board piece =
    let
        opposingPieces =
            List.filter (\p -> p.cColor /= piece.cColor) board.pieces

        friendlyPieces =
            List.filter (\p -> p.cColor == piece.cColor) board.pieces

        ( row, col ) =
            piece.pos

        possiblePositions =
            [ ( row + 2, col + 1 )
            , ( row + 2, col - 1 )
            , ( row + 1, col + 2 )
            , ( row + 1, col - 2 )
            , ( row - 1, col + 2 )
            , ( row - 1, col - 2 )
            , ( row - 2, col + 1 )
            , ( row - 2, col - 1 )
            ]
                |> List.filter validPos
                |> List.filter (\pos -> not (pieceAt friendlyPieces pos))

        possibleStates =
            List.map
                (\pos ->
                    case getPieceAt opposingPieces pos of
                        Just p ->
                            [ Capture p ]

                        _ ->
                            [ MoveTo pos ]
                )
                possiblePositions
    in
        possibleStates


getSuccesorStateKing : Board -> Piece -> List (List State)
getSuccesorStateKing board piece =
    let
        opposingPieces =
            List.filter (\p -> p.cColor /= piece.cColor) board.pieces

        friendlyPieces =
            List.filter (\p -> p.cColor == piece.cColor) board.pieces

        ( row, col ) =
            piece.pos

        possiblePositions =
            [ ( row + 1, col + 1 )
            , ( row + 1, col - 1 )
            , ( row + 1, col )
            , ( row, col + 1 )
            , ( row, col - 1 )
            , ( row - 1, col + 1 )
            , ( row - 1, col - 1 )
            , ( row - 1, col )
            ]
                |> List.filter validPos
                |> List.filter (\pos -> not (pieceAt friendlyPieces pos))

        captureStates =
            List.map
                (\pos ->
                    case getPieceAt opposingPieces pos of
                        Just p ->
                            [ Capture p ]

                        _ ->
                            [ MoveTo pos ]
                )
                possiblePositions

        -- Handle Castteling
        castleStates =
            if piece.rank == King False then
                let
                    castlingPieces =
                        List.filter (\p -> p.rank == Rook False) friendlyPieces

                    -- Check pieces between castling are fine
                    canCastle =
                        List.filter
                            (\castle ->
                                let
                                    ( cR, cC ) =
                                        castle.pos

                                    emptyCol =
                                        if cC < col then
                                            List.range (cC + 1) (col - 1)
                                        else
                                            List.range (col + 1) (cC - 1)

                                    emptyPos =
                                        List.map (\c -> ( cR, c )) emptyCol
                                in
                                    if List.any (pieceAt board.pieces) emptyPos then
                                        False
                                    else
                                        True
                            )
                            castlingPieces
                in
                    List.map
                        (\castle ->
                            [ CastleWith castle ]
                        )
                        canCastle
            else
                []
    in
        List.filter
            (\stateList ->
                let
                    ( newPiece, newBoard ) =
                        applyStates board piece stateList
                in
                    if inCheck newBoard newPiece then
                        False
                    else
                        True
            )
            (captureStates ++ castleStates)


getSuccesorStatePawn : Board -> Piece -> List (List State)
getSuccesorStatePawn board piece =
    let
        startingRow =
            if piece.cColor == White then
                1
            else
                6

        endingRow =
            if piece.cColor == White then
                7
            else
                0

        direction =
            if piece.cColor == White then
                1
            else
                -1

        opposingPieces =
            List.filter (\p -> p.cColor /= piece.cColor) board.pieces

        ( row, col ) =
            piece.pos

        -- Can pawn go forward
        goForward =
            if not (validPos ( row + direction, col )) || pieceAt board.pieces ( row + direction, col ) then
                []
            else if row + direction == endingRow then
                [ [ MoveTo ( row + direction, col ), Promote ] ]
            else
                [ [ MoveTo ( row + direction, col ) ] ]

        goForwardTwice =
            if row /= startingRow || pieceAt board.pieces ( row + 2 * direction, col ) then
                []
            else
                [ [ MoveTo ( row + direction, col ), JumpAhead ] ]

        -- Can pawn capture anything
        capturePositions =
            List.filter validPos [ ( row + direction, col - 1 ), ( row + direction, col + 1 ) ]

        capturedPieces =
            List.map (\pos -> ( pos, (getPieceAt opposingPieces pos) )) capturePositions

        captureStates =
            List.map
                (\( pos, piece ) ->
                    case piece of
                        Just p ->
                            [ MoveTo pos, Capture p ]

                        _ ->
                            []
                )
                capturedPieces
                |> List.filter (\state -> not (List.isEmpty state))

        -- Can pawn Empassant
        enPassantPositions =
            List.filter validPos [ ( row, col - 1 ), ( row, col + 1 ) ]

        enPassantPawns =
            List.filter (\piece -> (piece.rank == Pawn) && (List.member JumpAhead piece.lastState)) opposingPieces

        enPassantPieces =
            List.map (\pos -> ( pos, (getPieceAt enPassantPawns pos) )) enPassantPositions

        enPassantStates =
            if row /= (startingRow + 3 * direction) then
                []
            else
                List.map
                    (\( pos, piece ) ->
                        case piece of
                            Just p ->
                                [ MoveTo pos, Capture p ]

                            _ ->
                                []
                    )
                    enPassantPieces
                    |> List.filter (\state -> not (List.isEmpty state))
    in
        goForward ++ goForwardTwice ++ captureStates ++ enPassantStates
