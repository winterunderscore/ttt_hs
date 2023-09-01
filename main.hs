import Data.List
import Data.Char

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe"
  putStrLn "To make a move, type 'LN' where L is an uppercase letter A-C signifying the column and N is a number 1-3 signifying the row number"
  startGame

startGame :: IO ()
startGame = do
  let board = replicate 3 "   "
  putStrLn $ boardStr board
  gameLoop board 'X'

gameLoop :: Board -> Char -> IO ()
gameLoop board moving = do
  putStr $ "> " ++ [moving] ++ " to move: "
  input <- getLine

  let move = parseInput input
  if snd move && (getPos board $ fst move) == ' ' then do
    let newBoard = setPos board (fst move) moving
    putStrLn $ boardStr newBoard
    if hasWon newBoard (fst move) then do
      putStrLn $ [moving]++" has Won the Game!"
      endGame
    else if hasTied newBoard then do
      putStrLn "Tie!"
      endGame
    else do
      gameLoop newBoard (nextMove moving)
  else do
    putStrLn $ "Not a valid move"
    gameLoop board moving

endGame :: IO ()
endGame = do
  putStrLn "The Game has Ended"
  putStr "Would you like to restart (Y/N): "
  input <- getLine
  if input == "Y" then 
    startGame
  else
    pure ()

type Board = [[Char]]
type Position = (Int, Int) --x, y

boardStr :: Board -> String
boardStr board =
  columns ++ "\n"
  ++ intercalate "\n" [labelRow (board!!row) row | row <- [0..2]]
  where
    columns = ' ':intercalate " " ("":deepen ['A'..'C'])
    labelRow board row = 
      intercalate " " ((show (row+1)):[[x] | x <- board])

parseInput :: [Char] -> (Position, Bool)
parseInput input
  | length input /= 2 = illegal
  | (x `elem` ['A'..'C']) && (y `elem` ['1'..'3']) = (((ord x - 65), (ord y - 49)), True)
  | otherwise = illegal
  where
    x = input!!0
    y = input!!1
    illegal = ((0,0),False)

hasWon :: Board -> Position -> Bool
hasWon b m =
  vert || horiz ||
  dUL' || dUR'
  where
    dUL = diagUL b
    dUR = diagUR b
    vert = allEq $ b!!(snd m)
    horiz = allEq $ map (!! (fst m)) b
    dUL' = (not $ all (==' ') dUL) && (allEq dUL)
    dUR' = (not $ all (==' ') dUR) && (allEq dUR)

hasTied :: Board -> Bool
hasTied = not . foldr1 (||) . map (any(==' '))
  
deepen :: [a] -> [[a]]
deepen = map (\x -> [x])

nextMove :: Char -> Char
nextMove cur = if cur == 'X' then 'O' else 'X'

setRow :: [a] -> Int -> a -> [a]
setRow row pos to = 
  let a = splitAt pos row
  in fst a ++ to:(tail $ snd a)

setPos :: Board -> Position -> Char -> Board
setPos board (x,y) to = 
  setRow board y (setRow (board!!y) x to) 

getPos :: Board -> Position -> Char
getPos board (x,y) = board!!x!!y

allEq :: Eq a => [a] -> Bool
allEq (x:xs) = all (==x) xs

diagUR :: [[a]] -> [a]
diagUR xs = [(xs!!n)!!n | n <- [0..2]]

diagUL :: [[a]] -> [a]
diagUL xs = [(xs!!n)!!(2-n)| n <- [0..2]]
