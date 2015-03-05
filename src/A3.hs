module A3 where
    
    data PieceType = Pawn | Rook | Knight | Bishop | Queen | King

    instance Show PieceType where
        show Pawn   = "P "
        show Rook   = "R "
        show Knight = "Kn"
        show Bishop = "B "
        show Queen  = "Q "
        show King   = "K "

    --readPieceType :: String -> PieceType
    --readPieceType "P" = Pawn
    --readPieceType "R" = Rook
    --readPieceType "Kn" = Knight
    --readPieceType "B" = Bishop
    --readPieceType "Q" = Queen
    --readPieceType "K" = King

    data Color = Black | White

    instance Show Color where
        show Black = "B"
        show White = "W"

    readColor :: Char -> Color
    readColor 'B' = Black
    readColor 'W' = White

    
    data Piece = Piece (Color, PieceType)
    instance Show Piece where
        show (Piece (x,y)) = show x ++ show y
    instance Read Piece where
        --readPieceType (x:y) = [(Piece (readColor x, readPieceType y))]
        readsPrec _ (h:t) = [(Piece ((if h == 'B' then Black else if h == 'W' then White else error ("Parse error")), readPieceType t), "")] where
            readPieceType a | a == "P" = Pawn   
                            | a == "R" = Rook    
                            | a == "Kn" = Knight 
                            | a == "B" = Bishop  
                            | a == "Q" = Queen   
                            | a == "K" = King    
                            | otherwise = error ("Parse Error" ++ a)


    data Cell = Cell (Maybe Piece)
    instance Show Cell where
        show (Cell Nothing) = "   "
        show (Cell (Just x)) = show x

    readCell :: String -> Cell
    readCell "" = Cell Nothing
    readCell x = Cell (Just (read x))

    type Board = [[Cell]]

    backRow = ["R","Kn","B","Q","K","B","Kn","R"]
    
    chessBoardString = [
                    map ("B" ++) backRow,
                    replicate 8 "BP",
                    replicate 8 "", replicate 8 "",
                    replicate 8 "", replicate 8 "",
                    replicate 8 "WP",
                    map ("W" ++) backRow
                  ]

    

    parseRow :: [String] -> [Cell]
    parseRow [] = []
    parseRow (x:y) = readCell x : (parseRow y)

    parseBoard :: [[String]] -> Board
    parseBoard [] = []
    parseBoard (x:y) = (parseRow x) : (parseBoard y)

    makeBarString x y = take (8 * 4 + 1) (cycle (x ++ take 3 (y++y++y))) ++ "\n"
    barString = makeBarString "o" "~"

    showRow :: [Cell] -> String
    showRow [] = "|"
    showRow (x:y) = "|" ++ (show x) ++ showRow y

    showBoard :: [[Cell]] -> String
    showBoard [] = barString
    showBoard (x:y) = barString ++ showRow x ++ "\n" ++ showBoard y
    x = read "BKn" :: Piece
    l = length [1..]
    main = do
        --let p1 = (Black, Knight)
        --print (showPiece p1)
        --print (parseChessRow chessRow)
        --print (parseBoard chessBoardString)
        --putStr $ showBoard (parseBoard chessBoardString)
        --print (barString ++ barString)
        print x
        print (l)
        