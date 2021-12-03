module Main where

import           Data.List
import           Data.Char                      ( digitToInt )

findGamma :: [[Int]] -> [Int]
findGamma inp = toBinList $ foldl' (zipWith (+)) (repeat 0) inp
 where
  len       = length inp
  toBinList = map (`div` (len `div` 2))

findEpsilon :: [Int] -> [Int]
findEpsilon = map (1 -)

toDec :: [Int] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0

problem1 :: [[Int]] -> Int
problem1 inp = toDec gamma * toDec epsilon
 where
  gamma   = findGamma inp
  epsilon = findEpsilon gamma

-- part 2
mostCommon :: [[Int]] -> [Ordering]
mostCommon inp = toBinList $ foldl' (zipWith (+)) (repeat 0) inp
 where
  len       = length inp
  toBinList = map (\k -> compare (2 * k) len)

genRating :: Int -> Int -> [[Int]] -> Int
genRating _ _ [x] = toDec x
genRating eq i xs  = genRating eq (i + 1) $ filter (\l -> p $ l !! i) xs
 where
  commons = mostCommon xs
  p :: Int -> Bool
  p n
    | eq == 1 = n == if (commons !! i) == LT then 0 else 1
    | otherwise =  n
    == if (commons !! i) == GT || (commons !! i) == EQ then 0 else 1

oxygenGenRating :: Int -> [[Int]] -> Int
oxygenGenRating = genRating 1

co2scrubberRating :: Int -> [[Int]] -> Int
co2scrubberRating = genRating 0

problem2 :: [[Int]] -> Int
problem2 inp = oR * cR
 where
  oR      = oxygenGenRating 0 inp
  cR      = co2scrubberRating 0 inp

main :: IO ()
main = do
  inp <- map (map digitToInt) . lines <$> readFile "in03.txt"
  print $ problem1 inp
  print $ problem2 inp
