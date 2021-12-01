module Day19 where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Bifunctor

data Rule = Literal Char | Rules [[Int]]

parseRule :: String -> Map Int Rule
parseRule str = M.singleton ruleNr getRule
  where
    (ruleNr, _:_:r) = first read $ break (==':') str
    l = r!!1 
    (r1, r2) = both (map read . words) $ break (=='|') r
    both f = bimap f f
    getRule
      | '\"' `elem` str = Literal l
      | null r2 = Rules [r1] 
      | otherwise = Rules [r1, r2]

parseInput :: String -> (Map Int Rule, [String])
parseInput = first (M.unions . map parseRule) . break (=="") . lines

matches :: Map Int Rule -> Int -> String -> Bool 
matches m i q

  where
    rl = m M.! i
    matchOnce (Literal l) str = str == l:""
    matchOnce (Rules rls) str = any $ (matchSeq str) rls
    matchSeq str rls = 

day19 :: IO ()
day19 = do
  putStrLn "Day19"
  inp <- readFile "in19.txt"