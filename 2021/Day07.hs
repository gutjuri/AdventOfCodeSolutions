module Main where

import qualified Data.IntMap.Strict as M
import Debug.Trace

parseInput :: String -> M.IntMap Int
parseInput str = go M.empty $ read $ "[" ++ str ++ "]"
  where
    go m [] = m
    go m (x:xs) = go (M.insertWith (+) x 1 m) xs

getTotalFuelCosts :: Int -> M.IntMap Int -> Int 
getTotalFuelCosts n = sum . M.mapWithKey (\k v -> v * abs (n-k))

getTotalFuelCosts' :: Int -> M.IntMap Int -> Int 
getTotalFuelCosts' n = sum . M.mapWithKey (\k v -> v * abs (n-k) * (abs (n-k) + 1) `div` 2)

p1 :: M.IntMap Int -> Int 
p1 m = minimum $ M.mapWithKey (\k _ -> getTotalFuelCosts k m) m

p2 :: M.IntMap Int -> Int 
p2 m = minimum $ map (`getTotalFuelCosts'` m) [1..1000]

main :: IO () 
main = do
  inp <- parseInput <$> readFile "in07.txt"
  print $ p1 inp
  print $ p2 inp

