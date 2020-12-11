module Day11 where

import qualified Data.Array                    as A
import           Data.Array                     ( Array )
import           Data.List
import           Data.Maybe

data PosState = Floor | Empty | Occupied
  deriving (Eq, Show)

type WaitingArea = Array (Int, Int) PosState

maxBounds :: (Int, Int)
maxBounds = (97, 91)

showWA :: WaitingArea -> String
showWA wa =
  map (\p -> if fst p == (fst maxBounds + 1) then '\n' else s (wa A.! p))
    $ [ (x, y) | y <- [1 .. snd maxBounds], x <- [1 .. fst maxBounds + 1] ]
 where
  s Floor    = '.'
  s Empty    = 'L'
  s Occupied = '#'

parseInput :: String -> WaitingArea
parseInput str =
  A.array ((1, 1), (maxX, maxY))
    $ zip [ (x, y) | y <- [1 .. maxY], x <- [1 .. maxX] ]
    $ map readState
    $ filter (/= '\n') str
 where
  (maxX, maxY) = maxBounds
  readState '.' = Floor
  readState '#' = Occupied
  readState 'L' = Empty

step :: WaitingArea -> WaitingArea
step wa = wa A.// map nextState (A.assocs wa)
 where
  nextState (ind, _) | wa A.! ind == Floor                    = (ind, Floor)
                     | wa A.! ind == Empty && adjPop ind == 0 = (ind, Occupied)
                     | wa A.! ind == Occupied && adjPop ind >= 4 = (ind, Empty)
                     | otherwise = (ind, wa A.! ind)
  adjPop (x, y) = length $ filter
    t
    [ (x - 1, y - 1)
    , (x    , y - 1)
    , (x + 1, y - 1)
    , (x - 1, y)
    , (x + 1, y)
    , (x - 1, y + 1)
    , (x    , y + 1)
    , (x + 1, y + 1)
    ]
  (maxX, maxY) = maxBounds
  t i = isInd i && wa A.! i == Occupied
  isInd (x, y) = x > 0 && y > 0 && x <= maxX && y <= maxY

plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (a, b) (c, d) = (a + c, b + d)

step2 :: WaitingArea -> WaitingArea
step2 wa = wa A.// map nextState (A.assocs wa)
 where
  nextState (ind, _) | wa A.! ind == Floor                    = (ind, Floor)
                     | wa A.! ind == Empty && adjPop ind == 0 = (ind, Occupied)
                     | wa A.! ind == Occupied && adjPop ind >= 5 = (ind, Empty)
                     | otherwise = (ind, wa A.! ind)
  adjPop (x, y) = length $ filter ((== Occupied) . (wa A.!)) $ mapMaybe
    (seatInDir (x, y))
    [(1, -1), (1, 0), (1, 1), (0, -1), (0, 1), (-1, -1), (-1, 0), (-1, 1)]
  (maxX, maxY) = maxBounds
  seatInDir p dir =
    find ((/= Floor) . (wa A.!)) $ takeWhile isInd $ tail $ iterate (plus dir) p
  isInd (x, y) = x > 0 && y > 0 && x <= maxX && y <= maxY

untilStable :: (WaitingArea -> WaitingArea) -> WaitingArea -> WaitingArea
untilStable f = findAdjEq . iterate f
 where
  findAdjEq (x : y : xs) | x == y    = x
                         | otherwise = findAdjEq (y : xs)

countOcc :: WaitingArea -> Int
countOcc = length . filter (== Occupied) . A.elems

day11 :: IO ()
day11 = do
  putStrLn "Day11"
  inp <- parseInput <$> readFile "in11.txt"
  print $ countOcc $ untilStable step inp
  print $ countOcc $ untilStable step2 inp
