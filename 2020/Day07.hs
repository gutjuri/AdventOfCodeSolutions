module Day07 where

import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )

parseLine :: String -> Map String (Map String Int)
parseLine str = M.singleton parentCol ruleS
 where
  tokens    = words str
  parentCol = unwords $ take 2 tokens
  ruleS     = go $ drop 4 tokens
  go ("no"            : _ ) = M.empty
  go (n : c1 : c2 : _ : xs) = M.insert (c1 <> " " <> c2) (read n) (go xs)
  go []                     = M.empty

buildRuleset :: [String] -> Map String (Map String Int)
buildRuleset = M.unions . map parseLine

canReachShinyGold :: Map String (Map String Int) -> String -> Bool
canReachShinyGold rls n = M.member "shiny gold" rule
  || any (canReachShinyGold rls) (M.keys rule)
  where rule = M.findWithDefault M.empty n rls

mustContain :: Map String (Map String Int) -> String -> Int
mustContain rls n = M.foldrWithKey'
  (\childCol childN acc -> childN * (1 + mustContain rls childCol) + acc)
  0
  rule
  where rule = M.findWithDefault M.empty n rls

day07 :: IO ()
day07 = do
  putStrLn "Day07"
  inp <- lines <$> readFile "in07.txt"
  let rules = buildRuleset inp
  print $ length $ filter (canReachShinyGold rules) (M.keys rules)
  print $ mustContain rules "shiny gold"
