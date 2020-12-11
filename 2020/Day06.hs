module Day06 where

import           Data.List

parseInput :: String -> [[String]]
parseInput = groupBy (\a b -> null a == null b) . lines

p1 :: [[String]] -> Int
p1 = sum . map (length . foldl1 union)

p2 :: [[String]] -> Int
p2 = sum . map (length . foldl1 intersect)

day06 :: IO ()
day06 = do
  putStrLn "Day06"
  inp <- parseInput <$> readFile "in06.txt"
  print $ p1 inp
  print $ p2 inp
