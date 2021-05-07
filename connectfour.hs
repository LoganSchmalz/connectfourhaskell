import Text.Read

rows = 6
columns = 7
connect = 4
sep = replicate columns '-'
type Player = Char
p1 = 'X'
p2 = 'O'
type Row = [Char]
emptyRow = replicate columns ' '
type Board = [Row]

other :: Player -> Player
other p | p == p1   = p2
        | p == p2   = p1
        | otherwise = ' '

printBoard :: Board -> IO ()
printBoard b = do putStr (stringBoard b 0)

stringBoard :: Board -> Int -> String
stringBoard b r | r >= rows = ""
                | otherwise = b!!r ++ "\n" ++ stringBoard b (r+1)

checkWinHorz :: Board -> Int -> Int -> Player
checkWinHorz b r c
  | c <= columns - connect && r < rows = if player && equality b r c 1 then b!!r!!c else checkWinHorz b (r+1) c
  | c <= columns - connect             = checkWinHorz b 0 (c+1)
  | otherwise                          = ' '
  where equality b r c count
          | count >= connect = True
          | otherwise        = (b!!r!!c == b!!r!!(c+1)) && equality b r (c+1) (count+1)
        player = b!!r!!c == p1 || b!!r!!c == p2

checkWinVert :: Board -> Int -> Int -> Player
checkWinVert b r c
  | r <= rows - connect && c < columns = if player && equality b r c 1 then b!!r!!c else checkWinVert b r (c+1)
  | r <= rows - connect                = checkWinVert b (r+1) 0
  | otherwise                          = ' '
  where equality b r c count
          | count >= connect = True
          | otherwise        = (b!!r!!c == b!!(r+1)!!c) && equality b (r+1) c (count+1)
        player = b!!r!!c == p1 || b!!r!!c == p2

checkWinDiagR :: Board -> Int -> Int -> Player
checkWinDiagR b r c
  | c <= columns - connect = if player && equality b r c 1 then b!!r!!c else checkWinDiagR b r (c+1)
  | r <= rows - connect    = checkWinDiagR b (r+1) 0
  | otherwise              = ' '
  where equality b r c count
          | count >= connect = True
          | otherwise        = (b!!r!!c == b!!(r+1)!!(c+1)) && equality b (r+1) (c+1) (count+1)
        player = b!!r!!c == p1 || b!!r!!c == p2

checkWinDiagL :: Board -> Int -> Int -> Player
checkWinDiagL b r c
  | c >= columns - connect && c < columns = if player && equality b r c 1 then b!!r!!c else checkWinDiagL b r (c+1)
  | r <= rows - connect                   = checkWinDiagL b (r+1) (connect-1)
  | otherwise                             = ' '
  where equality b r c count
          | count >= connect = True
          | otherwise        = (b!!r!!c == b!!(r+1)!!(c-1)) && equality b (r+1) (c-1) (count + 1)
        player = b!!r!!c == p1 || b!!r!!c == p2

checkWin :: Board -> Player
checkWin b | not (horz == ' ') = horz
           | not (vert == ' ') = vert
           | not (diaR == ' ') = diaR
           | not (diaL == ' ') = diaL
           | otherwise         = ' '
           where horz = checkWinHorz b 0 0
                 vert = checkWinVert b 0 0
                 diaR = checkWinDiagR b 0 0
                 diaL = checkWinDiagL b 0 (connect-1)

dropC :: Board -> Player -> Int -> Board
dropC b p c = take r b ++ [newrow] ++ drop (r+1) b
  where r      = lowestRow b (rows-1) c
        lowestRow b y c -- | y < 0          = -1 --shouldn't happen based on checking in play func
                        | b!!y!!c == ' ' = y
                        | otherwise      = lowestRow b (y-1) c
        newrow = take c row ++ [p] ++ drop (c+1) row 
        row = b!!r
  

main :: IO()
main = do play (replicate 7 emptyRow) 'X'

play :: Board -> Player -> IO ()
play b p
  | checkWin b == p1 = do
      printBoard b
      putStrLn (p1 : " wins!")
  | checkWin b == p2 = do
      printBoard b
      putStrLn (p2: " wins!")
  | otherwise = do
      printBoard b
      putStrLn sep
      putStrLn (p : ", enter a column [1-" ++ show columns ++ "]:")
      input <- getLine
      let c = case (readMaybe input :: Maybe Int) of
                Just n  -> n-1
                Nothing -> -1
      putStrLn sep
      if (c >= 0) && (c <= columns) && (b!!0!!c == ' ')
        then do play (dropC b p c) (other p)
        else do putStrLn "?"
                play b p
