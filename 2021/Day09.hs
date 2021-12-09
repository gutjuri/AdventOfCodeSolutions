module Main where

import           Data.Array
import           Data.Char                      ( digitToInt )
import           Data.List                      ( sortOn )
import qualified Data.Set                      as S
import           Data.Ord

parseInput :: String -> Array (Int, Int) Int
parseInput str =
  listArray ((0, 0), (edgeLenY - 1, edgeLenX - 1)) $ map digitToInt $ filter
    (/= '\n')
    str
 where
  edgeLenY = length $ lines str
  edgeLenX = length $ head $ lines str


getNeighbourIndices :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
getNeighbourIndices ((minA, minB), (maxA, maxB)) (a, b) = filter
  (\(x, y) -> x >= minA && x <= maxA && y >= minB && y <= maxB)
  [(a + 1, b), (a - 1, b), (a, b + 1), (a, b - 1)]

isLowPoint :: Array (Int, Int) Int -> (Int, Int) -> Bool
isLowPoint arr ind =
  all (\ind' -> arr ! ind' > arr ! ind) $ getNeighbourIndices (bounds arr) ind

getBassins :: Array (Int, Int) Int -> [(Int, Int)]
getBassins arr = filter (isLowPoint arr) $ indices arr

getBassinSize :: Array (Int, Int) Int -> (Int, Int) -> Int
getBassinSize arr p = go S.empty [p]
 where
  go :: S.Set (Int, Int) -> [(Int, Int)] -> Int
  go vis (x : xs)
    | arr ! x == 9 = go vis xs
    | x `S.member` vis = go vis xs
    | otherwise = 1
    + go (x `S.insert` vis) (getNeighbourIndices (bounds arr) x ++ xs)
  go vis [] = 0


p1 :: Array (Int, Int) Int -> Int
p1 arr = sum $ map ((+ 1) . (arr !)) $ getBassins arr

p2 :: Array (Int, Int) Int -> Int
p2 arr =
  product $ take 3 $ sortOn Down $ map (getBassinSize arr) $ getBassins arr

main :: IO ()
main = do
  inp <- parseInput <$> readFile "in09.txt"
  print $ p1 inp
  print $ p2 inp

