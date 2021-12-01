module Main where

import           Data.List                      ( foldl' )

problem1 :: [Int] -> Int
problem1 = snd . foldl' f (maxBound, 0)
 where
  f (last, n) x | last < x  = (x, n + 1)
                | otherwise = (x, n)

problem2 :: [Int] -> Int
problem2 = problem1 . mkWindows
 where
  mkWindows (a : b : c : xs) = a + b + c : mkWindows (b : c : xs)
  mkWindows _                = []

main :: IO ()
main = do
  inp <- map read . lines <$> readFile "in01.txt"
  print $ problem1 inp
  print $ problem2 inp
