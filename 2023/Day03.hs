module Day03 where

import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Char
import Debug.Trace

addIndices :: String -> [((Int, Int), Char)]
addIndices str = zip ind $ concat ls
    where
        ls = lines str
        ind = [(i, j) | j <- [0..length ls - 1], i <-  [0 .. length (head ls) - 1]]

getAdj :: (Int, Int) -> [(Int, Int)]
getAdj (x, y) = [(x+dx, y+dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

parseInput :: String -> (M.Map (Int, Int) String, [(Int, [(Int, Int)])], S.Set (Int, Int))
parseInput str = (M.fromListWith (++) adjIndices, concatMap (uncurry getNumsInLine) $ zip [0..] $ lines str, S.fromList starsIndices) 
    where
        adjIndices = concatMap (\(p, s) -> zip (getAdj p) $ repeat s) $ mapMaybe (\(p, c) -> if isDigit c || c == '.' then Nothing else Just (p, c:"")) $ addIndices str
        starsIndices = mapMaybe (\(p, c) -> if c == '*' then Just p else Nothing) $ addIndices str

getNumsInLine :: Int -> String -> [(Int, [(Int, Int)])]
getNumsInLine lineNr = go 0 False []
    where
        go _ _ rs [] = rs
        go x False rs (l:ls)
            | isDigit l = go (x+1) True ((digitToInt l, [(x, lineNr)]):rs) ls
            | otherwise = go (x+1) False rs ls
        go x True r@((n, ps):rs) (l:ls)
            | isDigit l = go (x+1) True ((n*10 + digitToInt l, (x, lineNr):ps):rs) ls
            | otherwise = go (x+1) False r ls

task1 :: M.Map (Int, Int) String ->  [(Int, [(Int, Int)])] -> Int
task1 adjIndices nums = sum $ mapMaybe (\(n, xs) -> if any  (`M.member` adjIndices) xs then Just n else Nothing) nums

task2 :: S.Set (Int, Int) ->  [(Int, [(Int, Int)])] -> Int
task2 starsIndices nums = sum gearRatios
    where
        gearRatios = mapMaybe (\p -> let ad = adjNums p in if length ad == 2 then Just (product ad) else Nothing) $ S.toList starsIndices 
        adjNums p = let adj = getAdj p in mapMaybe (\(n, ps) -> if any (`elem` adj) ps then Just n else Nothing) nums

main :: IO ()
main = do
    inp <- readFile "2023/in03.txt"
    let (adjIndices, nums, stars) = parseInput inp
    print $ task1 adjIndices nums
    print $ task2 stars nums
