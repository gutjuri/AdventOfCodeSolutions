module Main where

import           Data.Char                      ( isNumber )
import           Data.List                      ( find )
import           Data.Maybe                     ( fromJust )

type Point = (Int, Int)

data Result = Hit | Overshot | Undershot deriving (Show, Eq)

parseInput :: String -> (Point, Point)
parseInput =
  (\[x1, y1, x2, y2] -> ((x1, x2), (y1, y2))) . map read . words . map
    (\x -> if isNumber x || x == '-' then x else ' ')

isOvershot :: (Point, Point) -> Point -> Bool
isOvershot ((_, _), (maxX, _)) (x, _) = x > maxX

isUndershot :: (Point, Point) -> Point -> Bool
isUndershot ((_, minY), _) (_, y) = y < minY

isInBounds :: (Point, Point) -> (Int, Int) -> Bool
isInBounds ((minX, minY), (maxX, maxY)) (x, y) =
  x <= maxX && x >= minX && y <= maxY && y >= minY

simulatePath :: (Point, Point) -> (Int, Int) -> Result
simulatePath tar (vX, vY) = go 0 0 vX vY
 where
  go x y vx vy | isOvershot tar (x, y)  = Overshot
               | isUndershot tar (x, y) = Undershot
               | isInBounds tar (x, y)  = Hit
               | otherwise = go (x + vx) (y + vy) (max 0 (vx - 1)) (vy - 1)

canHit :: (Point, Point) -> Int -> Int
canHit tar vy =
  length $ filter (== Hit) $ map (\vx -> simulatePath tar (vx, vy)) [0 .. 1000]

getMaxY :: (Point, Point) -> Maybe Int
getMaxY tar =
  (\x -> x * (x + 1) `div` 2) <$> find ((/= 0) . canHit tar) [1000, 999 .. 0]

getNrOfInitVel :: (Point, Point) -> Int
getNrOfInitVel tar = sum $ map (canHit tar) [2000, 1999 .. (-2000)]

main :: IO ()
main = do
  inp <- parseInput <$> readFile "in17.txt"
  print $ fromJust $ getMaxY inp
  print $ getNrOfInitVel inp
