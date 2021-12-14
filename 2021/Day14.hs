module Main where

import qualified Data.Map.Strict               as M
import           Data.List
import           Data.Bifunctor                 

type Rules = M.Map String Char
type Occurances = M.Map Char Integer
type LetterPairs = M.Map (Char, Char) Integer

parseInp :: String -> ((Occurances, LetterPairs), Rules)
parseInp =
  bimap ((\str -> (mkOccs str, strToMap str)) . head) (parseRules . drop 1)
    . splitAt 1
    . lines
 where
  parseRules = M.fromList . map parseRule
  parseRule  = bimap (delete ' ') last . break (== '-')
  mkOccs :: String -> Occurances
  mkOccs = M.fromList . map (\l -> (head l, genericLength l)) . group . sort

strToMap :: String -> LetterPairs
strToMap = M.fromListWith (+) . go
 where
  go (x : y : xs) = ((x, y), 1) : go (y : xs)
  go _            = []

applyRules :: Rules -> (Occurances, LetterPairs) -> (Occurances, LetterPairs)
applyRules r (nrs, str) = first (M.unionWith (+) nrs) $ M.foldlWithKey'
  (\(occs, lps) (x, y) n ->
    ( M.insertWith (+) (r M.! [x, y]) n occs
    , M.insertWith (+) (x, r M.! [x, y]) n
      $ M.insertWith (+) (r M.! [x, y], y) n lps
    )
  )
  (M.empty, M.empty)
  str

minMaxElement :: Occurances -> Integer
minMaxElement = (\l -> last l - head l) . sort . map snd . M.toList

simulateRounds :: Int -> Rules -> (Occurances, LetterPairs) -> Integer
simulateRounds n r = minMaxElement . fst . (!! n) . iterate (applyRules r)

main :: IO ()
main = do
  (str, rules) <- parseInp <$> readFile "in14.txt"
  print $ simulateRounds 10 rules str
  print $ simulateRounds 40 rules str
