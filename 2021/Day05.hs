module Main where

import qualified Data.Map.Strict               as M

type Point = (Int, Int)

parseInput :: String -> [(Point, Point)]
parseInput = map parseLine . lines
 where
  parseLine ln =
    let (p1, _ : _ : p2) = break (== '-') ln in (parseCoord p1, parseCoord p2)
  parseCoord c = let (c1, _ : c2) = break (== ',') c in (read c1, read c2)

isHorizonalOrVertical :: (Point, Point) -> Bool
isHorizonalOrVertical ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

getIntermediatePoints :: (Point, Point) -> [Point]
getIntermediatePoints ((x1, y1), (x2, y2))
  | x1 == x2  = [ (x1, k) | k <- [min y1 y2 .. max y1 y2] ]
  | y1 == y2  = [ (k, y2) | k <- [min x1 x2 .. max x1 x2] ]
  | x1 <x2   = [ (x1 + k, y1 `op` k) | k <- [0 .. abs (x1 - x2)] ]
  | otherwise = getIntermediatePoints ((x2, y2), (x1, y1))
  where op = if y1 > y2 then (-) else (+)

countDoubleCrossed :: [Point] -> Int
countDoubleCrossed = M.size . M.filter (> 1) . go M.empty
 where
  go m []       = m
  go m (p : ps) = go (M.insertWith (+) p 1 m) ps

problem1 :: [(Point, Point)] -> Int
problem1 =
  countDoubleCrossed
    . concatMap getIntermediatePoints
    . filter isHorizonalOrVertical

problem2 :: [(Point, Point)] -> Int
problem2 =
  countDoubleCrossed
    . concatMap getIntermediatePoints

main :: IO ()
main = do
  inp <- parseInput <$> readFile "in05.txt"
  print $ problem1 inp
  print $ problem2 inp
