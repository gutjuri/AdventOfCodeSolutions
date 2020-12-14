{-# LANGUAGE TupleSections #-}

module Day13 where

import Data.Maybe
import Data.Bifunctor
import Text.Read

parseInp :: String -> [Maybe Int]
parseInp = map readMaybe . words . map (\c -> if c == ',' then ' ' else c) 

closestBus :: Int -> [Maybe Int] -> (Int, Int)
closestBus ts = minimum . map (\t -> (t - ts `mod` t, t))  . catMaybes

indexBusNrs :: [Maybe Int] -> [(Int, Int)]
indexBusNrs = catMaybes . zipWith (\i mb -> (i,) <$> mb) [0 ..]

extendedEu :: Integer -> Integer -> (Integer, Integer)
extendedEu a 0 = (1, 0)
extendedEu a b = (t, s - q * t)
  where (q, r) = quotRem a b
	      (s, t) = extendedEu b r

day13 :: IO ()
day13 = do
  putStrLn "Day13"
  (depTime, busses) <- bimap read parseInp . break (=='\n') <$> readFile "in13.txt"
  print $  uncurry (*) $ closestBus depTime busses

