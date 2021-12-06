module Main where

import qualified Data.Map.Strict               as M

parseInput :: String -> [Int]
parseInput str = read $ "[" ++ str ++ "]"

toMap :: [Int] -> M.Map Int Int
toMap = go M.empty
 where
  go m (x : xs) = go (M.insertWith (+) x 1 m) xs
  go m []       = m

stepOnce :: M.Map Int Int -> M.Map Int Int
stepOnce = M.foldrWithKey f M.empty
 where
  f 0 a acc = M.insert 8 a $ M.insertWith (+) 6 a acc
  f k a acc = M.insertWith (+) (k - 1) a acc

simulateDays :: Int -> [Int] -> Int
simulateDays d = M.foldl (+) 0 . (!! d) . iterate stepOnce . toMap

main :: IO ()
main = do
  inp <- parseInput <$> readFile "in06.txt"
  print $ simulateDays 80 inp
  print $ simulateDays 256 inp
