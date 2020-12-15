{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day15 where

import Prelude hiding (replicate)
import Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed
import Control.Monad.ST
import Data.STRef


memoryNums :: Int -> [Int] -> UArray Int Int
memoryNums len startingNums = runSTUArray $ do
  lastR <- newSTRef (last startingNums)
  lastFreq <- newArray (0, len) (0 :: Int)
  forM_ [1 .. length startingNums - 1] $ \i -> do
    writeArray lastFreq (startingNums !! (i-1)) i
  forM_ [length startingNums .. len-1] $ \i -> do
    last <- readSTRef lastR
    i' <- readArray lastFreq last
    case i' of
      0 -> writeSTRef lastR 0
      x -> writeSTRef lastR (i-i')
    writeArray lastFreq last i
  last <- readSTRef lastR
  writeArray lastFreq 0 last
  return lastFreq
 where
  last xs = xs !! (length xs - 1)

day15 :: IO ()
day15 = do
  putStrLn "Day15"
  inp <- map read . words . map (\c -> if c == ',' then ' ' else c) <$> readFile "in15.txt"
  print $ memoryNums 2020 inp ! 0
  print $ memoryNums 30000000 inp ! 0
