module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.List (break, intersect)
import Data.Monoid

type Planet = String

main :: IO ()
main = do
  planets <- parseDeps
  let pathLength to = (getSum . depsTo to planets (\_ -> Sum 1))
  print $ sum $ map (pathLength "COM") $ M.keys planets
  let getPath = depsTo "COM" planets (:[])
      pathA = getPath "YOU"
      pathB = getPath "SAN"
      commonPlanet = head $ intersect pathA pathB
  print $ (pathLength commonPlanet "YOU") + (pathLength commonPlanet "SAN") - 2

depsTo :: Monoid a => Planet -> Map Planet Planet -> (Planet -> a) -> Planet -> a
depsTo t m x p | p == t    = mempty
               | otherwise = (x p) `mappend` (depsTo t m x (m M.! p))

parseDeps :: IO (Map Planet Planet)
parseDeps = M.fromList . map (\(c, o) -> (tail o, c)) . map (break (==')')) . lines <$> readFile "inputs/input06.txt" 
