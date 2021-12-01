{-# LANGUAGE TupleSections #-}

module Day13 where

import           Data.Maybe
import           Data.Bifunctor
import           Text.Read

parseInp :: String -> [Maybe Integer]
parseInp = map readMaybe . words . map (\c -> if c == ',' then ' ' else c)

closestBus :: Integer -> [Maybe Integer] -> (Integer, Integer)
closestBus ts = minimum . map (\t -> (t - ts `mod` t, t)) . catMaybes

indexBusNrs :: [Maybe Integer] -> [(Integer, Integer)]
indexBusNrs = map (\(i, b) -> ((b - i) `mod` b, b)) . catMaybes . zipWith
  (\i mb -> (i, ) <$> mb)
  [0 ..]

findSol :: [(Integer, Integer)] -> Integer
findSol = go 0 1
 where
  go time factor (x@(ind, busnr) : xs)
    | time `mod` busnr == ind = go time (factor * busnr) xs
    | otherwise               = go (time + factor) factor (x : xs)
  go time _ _ = time


day13 :: IO ()
day13 = do
  putStrLn "Day13"
  (depTime, busses) <- bimap read parseInp . break (== '\n') <$> readFile
    "in13.txt"
  print $ uncurry (*) $ closestBus depTime busses
  print $ findSol $ indexBusNrs busses

