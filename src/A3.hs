{-# LANGUAGE DeriveDataTypeable #-}
module A3 where
    import Data.List.Split
    import Data.Char
    import Data.Data
    data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Data, Typeable)
    
    instance Show PieceType where
        show Pawn   = "P "
        show Rook   = "R "
        show Knight = "Kn"
        show Bishop = "B "
        show Queen  = "Q "
        show King   = "K "
    instance Eq PieceType where
        Pawn == Pawn   = True
        Rook == Rook   = True
        Knight == Knight = True
        Bishop == Bishop = True
        Queen == Queen  = True
        King == King   = True
        _ == _ = False
    --readPieceType :: String -> PieceType
    --readPieceType "P" = Pawn
    --readPieceType "R" = Rook
    --readPieceType "Kn" = Knight
    --readPieceType "B" = Bishop
    --readPieceType "Q" = Queen
    --readPieceType "K" = King

    data Color = Black | White deriving (Data, Typeable)

    instance Show Color where
        show Black = "B"
        show White = "W"
    instance Eq Color where
        Black == Black = True
        White == White = True
        _ == _ = False
        x /= y = not (x == y)
    
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

    --type Board = [[Cell]]
    data Board = Board [[Cell]] [Piece] [Piece]
    type Coord = (Int,Int)
    type Dir = (Int,Int)

    backRow = ["R","Kn","B","Q","K","B","Kn","R"]
    
    chessBoardString = [
                    map ("W" ++) backRow,
                    replicate 8 "WP",
                    replicate 8 "", replicate 8 "",
                    replicate 8 "", replicate 8 "",
                    replicate 8 "BP",
                    map ("B" ++) backRow
                  ]

    parseRow :: [String] -> [Cell]
    parseRow [] = []
    parseRow (x:y) = readCell x : (parseRow y)

    parseBoard :: [[String]] -> Board
    parseBoard [] = (Board [] [] [])
    parseBoard x = (Board (parse x) [] [])
                where parse [] = []
                      parse (x:y) = (parseRow x) : (parse y)

    makeBarString x y = " " ++ take (8 * 4 + 1) (cycle (x ++ take 3 (y++y++y))) ++ "\n"
    barString = makeBarString "o" "~"
    makeLetterString :: Int -> String
    makeLetterString 0 = ""
    makeLetterString x = (makeLetterString (x-1)) ++ "   " ++ [(chr ((ord 'a') + x - 1))]

    showRow :: [Cell] -> String
    showRow [] = "|"
    showRow (x:y) = "|" ++ (show x) ++ showRow y

    showBoard :: Board -> String
    showBoard (Board [] _ _) = barString
    showBoard (Board (x:y) a b) = showBoard (Board y a b) ++ (show (8 - length y)) ++ showRow x ++ "\n" ++ barString ++ (if (length y) == 7 then (makeLetterString 8 ++ "\n") else "")

    charIndex :: Char -> Int
    charIndex c = ord c - ord 'a'

    dirMapping :: Char -> (Int,Int)
    dirMapping 'N' = (0,1)
    dirMapping 'S' = (0,-1)
    dirMapping 'W' = (-1,0)
    dirMapping 'E' = (1,0)
    dirMapping _ = (0,0)
    
    dirOffset :: String -> (Int, Int)
    dirOffset "" = (0,0)
    dirOffset (h:t) = addOffset (dirMapping h) (dirOffset t)
                    where addOffset (x,y) (z,w) = (x+z,y+w)
    
    posCoord :: String -> (Int, Int)
    posCoord "" = (-1,-1)
    posCoord s = (charIndex (s!!0), digitToInt (s!!1) - 1)

    

    getCell :: Coord -> Board -> Cell
    getCell (cx,cy) (Board x _ _) = (x !! cy) !! cx

    isMoveValid :: Piece -> Dir -> Bool
    isMoveValid (Piece (c,p)) (a,b) | p == Knight = (x == 2 && y == 1) || (x == 1 && y == 2)
                                    | x > 1 || y > 1 = False
                                    | p == King = x < 2 && y < 2
                                    | p == Rook = x == 0 || y == 0
                                    | p == Bishop = x /= 0 && y /= 0
                                    | p == Queen = True
                                    | p == Pawn = (x == 0 && y == 1 && (c /= Black || b == (-1)) && (c /= White || b == 1))
                                    where x = abs a
                                          y = abs b

    --testMove :: Coord -> Board -> Board
    --testMove (cx,cy) (bh:bt) = 

    replace :: Int -> x -> [x] -> [x]
    replace n e l = let (xs,ys) = splitAt n l in xs ++ [e] ++ (tail ys)

    replaceInGrid :: Coord -> Cell -> Board -> Board
    replaceInGrid (cx,cy) elem (Board grid x y) = let row = grid !! cy
                                                      newrow = replace cx elem row
                                                      in (Board (replace cy newrow grid) x y)

    moveSingle :: Coord -> Coord -> Board -> Board
    moveSingle (cx,cy) (c2x,c2y) grid = replaceInGrid (cx,cy) (Cell Nothing) (replaceInGrid (c2x,c2y) (getCell (cx,cy) grid) grid)

    --  cx < 0 || cx >= 8 = error "board movement error"
    makeMove :: Coord -> Dir -> Int -> Board -> Board
    makeMove _ _ 0 board = board
    makeMove (cx,cy) (dx,dy) n (Board brd w b) | ax < 0 || ax >= 8 = (Board brd w b)
                                     | ay < 0 || ay >= 8 = (Board brd w b)
                                     | otherwise = let origcell = getCell (cx,cy) (Board brd w b)
                                                       nextcell = getCell (cx+dx,cy+dy) (Board brd w b)
                                                       in case origcell of Cell Nothing -> (Board brd w b)
                                                                           Cell (Just (Piece (col,p))) -> let origcol = col
                                                                                                              origpiece = p
                                                                                                              movesleft = if (origpiece == Pawn || origpiece == Knight || origpiece == King) then 1 else n
                                                                                                              in if not (isMoveValid (Piece(col,p)) (dx,dy)) then (Board brd w b) else
                                                                                                              case nextcell of Cell Nothing -> makeMove (cx+dx,cy+dy) (dx,dy) (movesleft-1) (moveSingle (cx,cy) (cx+dx,cy+dy) (Board brd w b))
                                                                                                                               Cell (Just (Piece(col2, p2))) -> if origcol == col2 then (Board brd w b)
                                                                                                                               else moveSingle (cx,cy) (cx+dx,cy+dy) (if origcol == White then (Board brd (w ++ [Piece(col2,p2)]) b) else (Board brd w (b ++ [Piece(col2,p2)])))--capture
                                     where ax = cx + dx
                                           ay = cy + dy

    showPieces :: [Piece] -> String
    showPieces [] = []
    showPieces ((Piece(h1,h2)):t) = (show $ toConstr h1) ++ (show $ toConstr h2) ++ (if length t /= 0 then "," else "") ++ showPieces t

    inputLoop (Board brd w b) = do
        --putStr $ showBoard board
        input <- getLine
        let strings = splitOn " " input
        let coord = posCoord (strings!!0)
        let dir = dirOffset (strings!!1)
        let moves = if (length strings == 3) then (read (strings!!2) :: Int) else 7
        let (Board brd2 w2 b2) = makeMove coord dir moves (Board brd w b)
        putStr $ showBoard (Board brd2 w2 b2)
        putStr $ "White captures: " ++ (showPieces w2) ++ "\n"
        putStr $ "Black captures: " ++ (showPieces b2) ++ "\n"
        inputLoop (Board brd2 w2 b2)
    
    --x = read "BKn" :: Piece
    main = do
        print Black
        --putStr $ show $ isMoveValid (Piece (White,Pawn)) (0,1)
        --let p1 = (Black, Knight)
        --print (showPiece p1)
        --print (parseChessRow chessRow)
        --print (parseBoard chessBoardString)
        let board = parseBoard chessBoardString
        putStr $ showBoard board
        inputLoop board
        {-|
        input <- getLine ;
        let strings = splitOn " " input
        let coord = posCoord (strings!!0)
        let dir = dirOffset (strings!!1)
        let moves = if (length strings == 3) then (read (strings!!2) :: Int) else 1
        print $ coord
        print $ dir
        print $ moves
        --print $ getCell coord board

        let newboard = makeMove coord dir moves board
        putStr $ showBoard newboard
        --print $ dirOffset input
        --print $ splitOn " " input
        --print $ charIndex 'b'
        --putStr ("a: " ++ input)
        -}
        