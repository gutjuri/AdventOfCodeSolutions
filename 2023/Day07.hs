module Day07 where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List

tally :: String -> M.Map Integer Integer
tally = M.fromList . flip zip (repeat 1) . M.elems . M.fromListWith (+) . flip zip (repeat 1)

getStrength :: String -> Integer
getStrength xs
  | 5 `M.member` hist = 7
  | 4 `M.member` hist = 6
  | 3 `M.member` hist && 2 `M.member` hist = 5
  | 3 `M.member` hist = 4
  | 2 `M.member` hist && hist M.! 2 == 2 = 3
  | 2 `M.member` hist = 2
  | otherwise = 1
  where
    hist = tally xs

individualStrengths :: M.Map Char Int
individualStrengths = M.fromList $ zip  "AKQJT98765432" [0, -1 ..]

compareHands :: String -> String -> Ordering
compareHands a b = (\(x, y) -> compare (individualStrengths M.! x) (individualStrengths M.! y)) 
                    $ head $ dropWhile (uncurry (==)) $ zip a b

sorter :: (String, Integer, Integer) -> (String, Integer, Integer) -> Ordering
sorter (h1, s1, _) (h2, s2, _)
    | c1 /= EQ = c1
    | otherwise = compareHands h1 h2
    where
        c1 = compare s1 s2

parseInp :: String -> [(String, Integer, Integer)]
parseInp = map parseLine . lines
  where
    parseLine = (\(hand, bet) -> (hand, getStrength hand, read $ drop 1 bet)) . break (== ' ')

task1 :: [(String, Integer, Integer)] -> Integer
task1 = sum . zipWith (*) [1..] . map (\(_, _, b) -> b) . sortBy sorter

main :: IO ()
main = do
  inp <- parseInp <$> readFile "2023/in07.txt"
  print $ task1 inp