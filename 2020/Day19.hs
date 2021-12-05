module Day19 where

import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import           Data.Bifunctor
import           Data.List

data Rule = Literal Char | Rules [[Int]] deriving Show

parseRule :: String -> Map Int Rule
parseRule str = M.singleton ruleNr (getRule r)
 where
  (ruleNr, _ : _ : r) = first read $ break (== ':') str
  both f = bimap f f
  getRule c | '\"' `elem` str = Literal $ c !! 1
            | otherwise       = Rules $ go c
  go :: String -> [[Int]]
  go c | not ("|" `isInfixOf` c) = [map read $ words c]
  go c =
    let (rls, _ : rest) = first words $ break (== '|') c
    in  map read rls : go rest

tail' :: [a] -> [a]
tail' (x : xs) = xs
tail' []       = []

parseInput :: String -> (Map Int Rule, [String])
parseInput = bimap (M.unions . map parseRule) tail . break (== "") . lines

matchRule :: Map Int Rule -> String -> [Rule] -> Bool
matchRule m expr stack | length stack > length expr = False
                       | null stack || null expr    = null stack && null expr
matchRule m (e : expr) ((Literal c) : sx) = e == c && matchRule m expr sx
matchRule m expr ((Rules rls) : sx) = any (\r -> matchRule m expr (r ++ sx))
                                          rsets
  where rsets = map (map (m M.!)) rls

p1 :: Map Int Rule -> [String] -> Int
p1 m = length . filter
  (\str -> matchRule m str ((\(Rules x) -> map (m M.!) $ (x !! 0)) (m M.! 0)))

day19 :: IO ()
day19 = do
  putStrLn "Day19"
  (rules, instr) <- parseInput <$> readFile "in19.txt"
  print $ p1 rules instr
  let m' = M.insert 8 (Rules [[42], [42, 8]])
        $ M.insert 11 (Rules [[42, 31], [42, 11, 31]]) rules
  print $ p1 m' instr
