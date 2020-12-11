module Day09 where

import           Data.List
import           Data.Bifunctor
import           Data.Maybe

prefixLen :: Int
prefixLen = 25

sumOf25Prev :: [Int] -> Bool
sumOf25Prev xs = go pref
 where
  (pref, nr) = second head $ splitAt prefixLen xs
  go []        = False
  go (x : xs') = (nr - x) `elem` xs' || go xs'

isSumRange :: Int -> [Int] -> Maybe [Int]
isSumRange targ (x : xs) = go [x] x xs
 where
  go _ _ [] = Nothing
  go sval csum (l : ls) | csum + l > targ  = Nothing
                        | csum + l == targ = Just (l : sval)
                        | otherwise        = go (l : sval) (csum + l) ls

day09 :: IO ()
day09 = do
  putStrLn "Day09"
  inp <- map read . lines <$> readFile "in09.txt"
  let solution1 = fromJust (find (not . sumOf25Prev) $ tails inp) !! prefixLen
  print solution1
  let solution2 = head $ mapMaybe (isSumRange solution1) $ tails inp
  print (minimum solution2 + maximum solution2)
