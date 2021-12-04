module Main where

import           Data.List
import           Data.Bifunctor

data Board = Board {
  horizontal ::[[Int]],
  vertical :: [[Int]]
}
 deriving Show

parseInput :: String -> ([Int], [Board])
parseInput =
  bimap parseNumSq (filter (not . null . vertical) . parseBoards . lines)
    . break (== '\n')
 where
  parseNumSq = map read . words . map (\c -> if c == ',' then ' ' else c)
  parseBoards [] = []
  parseBoards str =
    let (b, rest) = break (== "") str
    in  parseBoard b : parseBoards (tail' rest)
  parseBoard str =
    let hz = map (map read . words) str
    in  Board { horizontal = hz, vertical = transpose hz }

tail' :: [a] -> [a]
tail' []       = []
tail' (x : xs) = xs

playRound :: Int -> [Board] -> [Board]
playRound n = map
  (\b -> b { horizontal = map (delete n) (horizontal b)
           , vertical   = map (delete n) (vertical b)
           }
  )

finished :: [Board] -> Maybe Board
finished = find isFinished

isFinished :: Board -> Bool
isFinished b = any null (vertical b) || any null (horizontal b)

firstBoardFinished :: [Int] -> [Board] -> (Int, Board)
firstBoardFinished (x : xs) bs = case finished nextBoards of
  Nothing -> firstBoardFinished xs nextBoards
  Just b  -> (x, b)
  where nextBoards = playRound x bs

boardSum :: Board -> Int
boardSum = sum . map sum . horizontal

problem1 :: [Int] -> [Board] -> Int
problem1 xs bs = n * boardSum board
  where (n, board) = firstBoardFinished xs bs

lastBoardFinished :: [Int] -> [Board] -> (Int, Board)
lastBoardFinished (x : xs) [b] =
  let [ns] = playRound x [b]
  in  if isFinished ns then (x, ns) else lastBoardFinished xs [ns]
lastBoardFinished (x : xs) bs =
  let ns = playRound x bs
  in  lastBoardFinished xs $ filter (not . isFinished) ns

problem2 :: [Int] -> [Board] -> Int
problem2 xs bs = n * boardSum board where (n, board) = lastBoardFinished xs bs

main :: IO ()
main = do
  (instr, boards) <- parseInput <$> readFile "in04.txt"
  print $ problem1 instr boards
  print $ problem2 instr boards
