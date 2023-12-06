module Day05 where

import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

newtype Mapping = Mapping (Integer, Integer, Integer)
  deriving (Show)

type Filter = [Mapping]

parseInput :: String -> ([Integer], [Filter])
parseInput = bimap parseSeeds parseFilters . fromJust . uncons . splitOn "\n\n"

parseSeeds :: String -> [Integer]
parseSeeds = map read . words . last . splitOn ": "

parseFilters :: [String] -> [Filter]
parseFilters = map (map readMapping . tail . lines)

readMapping :: String -> Mapping
readMapping = (\[a, b, c] -> Mapping (a, b, c)) . map read . words

applyFilter :: Integer -> Filter -> Integer
applyFilter n ms = maybe n (applyMapping n) (find (\(Mapping (destSt, srcSt, len)) -> srcSt <= n && srcSt + len > n) ms)
  where
    applyMapping n' (Mapping (destSt, srcSt, _)) = n' - srcSt + destSt

applyFilters :: Integer -> [Filter] -> Integer
applyFilters = foldl applyFilter

task1 :: ([Integer], [Filter]) -> Integer
task1 (seeds, filters) = minimum $ map (`applyFilters` filters) seeds

seedsToSeedRanges :: [Integer] -> [(Integer, Integer)]
seedsToSeedRanges = map (\[a, b] -> (a, b)) . chunksOf 2

getInterestingNums :: [Filter] -> [Integer]
getInterestingNums = concatMap getInterestingNums'
  where
    getInterestingNums' :: Filter -> [Integer]
    getInterestingNums' = concatMap getInterestingNums''
    getInterestingNums'' :: Mapping -> [Integer]
    getInterestingNums'' (Mapping (stDest, stSrc, len)) = concat [[stDest + x, stSrc + x, stSrc + len + x, stDest + len + x] | x <- [0, 1]]

task2 :: ([(Integer, Integer)], [Filter]) -> Integer
task2 (seedRanges, filters) = minimum $ map (`applyFilters` filters) iNums
  where
    iNums = filter (\x -> any (\(rs, rl) -> rs <= x && rs + rl > x) seedRanges) $ getInterestingNums filters

main :: IO ()
main = do
  inp@(seeds, filters) <- parseInput <$> readFile "2023/in05.txt"
  print $ task1 inp
  let seedRanges = seedsToSeedRanges seeds
  print $ task2 (seedRanges, filters)