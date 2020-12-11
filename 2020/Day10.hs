module Day10 where

import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )

p1 :: [Int] -> Int
p1 = (\(j1, j3, _) -> j1 * (j3 + 1)) . foldl' f (0, 0, 0)
 where
  f (j1, j3, pr) n | n - pr == 1 = (j1 + 1, j3, n)
                   | n - pr == 3 = (j1, j3 + 1, n)
                   | otherwise   = (j1, j3, n)

countw :: Int -> [Int] -> Map Int Int -> (Int, Map Int Int)
countw cs [x] m = (if cs + 3 < x then 0 else 1, m)
countw cs (x : xs) m
  | cs + 3 < x
  = (0, m)
  | cs `M.member` m
  = (m M.! cs, m)
  | otherwise
  = let (s1, m' ) = countw x xs m
        (s2, m'') = countw cs xs m'
    in  (s1 + s2, M.insert cs (s1 + s2) m'')

day10 :: IO ()
day10 = do
  putStrLn "Day10"
  inp <- sort . map read . lines <$> readFile "in10.txt"
  print $ p1 inp
  print $ fst $ countw 0 inp M.empty
