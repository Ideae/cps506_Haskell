--Name:   Zachary Harris
--Course: CPS506, Winter 2015, Assignment #3
--Due:    2015.03.09 23.59
--Credit: This is entirely my own work.
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

    
    data Cell = Piece (Color, PieceType) | None
    instance Show Cell where
        show None = "   "
        show (Piece (x,y)) = show x ++ show y
    instance Read Cell where
        readsPrec _ "" = [(None,"")]
        readsPrec _ (h:t) = [(Piece ((if h == 'B' then Black else if h == 'W' then White else error ("Parse error")), readPieceType t), "")] where
            readPieceType a | a == "P" = Pawn   
                            | a == "R" = Rook    
                            | a == "Kn" = Knight 
                            | a == "B" = Bishop  
                            | a == "Q" = Queen   
                            | a == "K" = King    
                            | otherwise = error ("Parse Error" ++ a)

    getCellPiece :: Cell -> PieceType
    getCellPiece None = error "cell does not contain piece"
    getCellPiece (Piece (c,p)) = p
    getCellColor :: Cell -> Color
    getCellColor None = error "cell does not contain piece"
    getCellColor (Piece (c,p)) = c
    isCellEmpty None = True
    isCellEmpty _ = False

    data Board = Board [[Cell]] [Cell] [Cell]

    getBoardGrid (Board grid _ _) = grid
    getBoardWCaptures (Board _ w _) = w
    getBoardBCaptures (Board _ _ b) = b
    addCapture (Board br w b) (Piece (c,p)) = if c == Black then (Board br (w ++ [Piece (c,p)]) b) else (Board br w (b ++ [Piece (c,p)]))

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
    parseRow (x:y) = (read x :: Cell) : (parseRow y)

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

    isMoveValid :: Cell -> Dir -> Bool
    isMoveValid (Piece (c,p)) (a,b) | p == Knight = (x == 2 && y == 1) || (x == 1 && y == 2)
                                    | x > 1 || y > 1 = False
                                    | p == King = x < 2 && y < 2
                                    | p == Rook = x == 0 || y == 0
                                    | p == Bishop = x /= 0 && y /= 0
                                    | p == Queen = True
                                    | p == Pawn = (x == 0 && y == 1 && (c /= Black || b == (-1)) && (c /= White || b == 1))
                                    where x = abs a
                                          y = abs b

    replace :: Int -> x -> [x] -> [x]
    replace n e l = let (xs,ys) = splitAt n l in xs ++ [e] ++ (tail ys)

    replaceInGrid :: Coord -> Cell -> Board -> Board
    replaceInGrid (cx,cy) elem (Board grid x y) = let row = grid !! cy
                                                      newrow = replace cx elem row
                                                      in (Board (replace cy newrow grid) x y)

    moveSingle :: Coord -> Coord -> Board -> Board
    moveSingle (cx,cy) (c2x,c2y) grid = replaceInGrid (cx,cy) None (replaceInGrid (c2x,c2y) (getCell (cx,cy) grid) grid)

    makeMove :: Coord -> Dir -> Int -> Board -> Board
    makeMove _ _ 0 board = board
    makeMove (cx,cy) (dx,dy) n board = if (ax < 0 || ax >= 8 || ay < 0 || ay >= 8) then board else
                                       let origcell = getCell (cx,cy) board
                                           nextcell = getCell (ax,ay) board
                                           in if (isCellEmpty origcell) then board else
                                           let origpiece = getCellPiece origcell
                                               origcol = getCellColor origcell
                                               movesleft = if (origpiece == Knight || origpiece == King) then 1 else n
                                               in if (origpiece == Pawn) then makeMovePawn (cx,cy) (dx,dy) movesleft origcell nextcell board else 
                                               if not (isMoveValid (Piece(origcol,origpiece)) (dx,dy)) then board else
                                               case nextcell of None -> makeMove (ax,ay) (dx,dy) (movesleft-1) (moveSingle (cx,cy) (ax,ay) board)
                                                                (Piece(col2, p2)) -> if origcol == col2 then board
                                                                else moveSingle (cx,cy) (ax,ay) (addCapture board (Piece(col2,p2)))
                                       where ax = cx + dx
                                             ay = cy + dy
    makeMovePawn :: Coord -> Dir -> Int -> Cell -> Cell -> Board -> Board
    makeMovePawn (cx,cy) (dx,dy) n origcell nextcell board = let col = getCellColor origcell
                                                                 col2 = getCellColor nextcell
                                                                 properDir = if col == Black then (-1) else (1)
                                                                 properStartPos = if col == Black then 6 else 1
                                                                 movesleft = if (dx /= 0) then 1 else if (cy == properStartPos) then (min n 2) else 1
                                                                 in if (dy /= properDir) then board else
                                                                 if (dx == 0) then (if (not (isCellEmpty nextcell)) then board else makeMove (ax,ay) (dx,dy) (movesleft-1) (moveSingle (cx,cy) (ax,ay) board)) else
                                                                 if ((abs dx) > 1) then board else
                                                                 if (col2 == col) then board else moveSingle (cx,cy) (ax,ay) (addCapture board nextcell)
                                                                 where ax = cx+dx
                                                                       ay = cy+dy

    showPieces :: [Cell] -> String
    showPieces [] = []
    showPieces ((Piece(h1,h2)):t) = (show $ toConstr h1) ++ (show $ toConstr h2) ++ (if length t /= 0 then "," else "") ++ showPieces t

    inputLoop (Board brd w b) = do
        input <- getLine
        if input == ""
            then do putStr $ showBoard (Board brd w b)
                    putStr $ "White captures: " ++ (showPieces w) ++ "\n"
                    putStr $ "Black captures: " ++ (showPieces b) ++ "\n"
            else do let strings = splitOn " " input
                    let coord = posCoord (strings!!0)
                    let dir = dirOffset (strings!!1)
                    let moves = if (length strings == 3) then (read (strings!!2) :: Int) else 7
                    let (Board brd2 w2 b2) = makeMove coord dir moves (Board brd w b)
                    --putStr $ showBoard (Board brd2 w2 b2)
                    --putStr $ "White captures: " ++ (showPieces w2) ++ "\n"
                    --putStr $ "Black captures: " ++ (showPieces b2) ++ "\n"
                    inputLoop (Board brd2 w2 b2)
    
    main = do
        let board = parseBoard chessBoardString
        --putStr $ showBoard board
        inputLoop board
        