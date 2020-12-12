module Day12 where

import           Data.List

type Dir = (Int, Int)

data Position = Position {
  dir :: Dir,
  x :: Int,
  y :: Int
} deriving (Show)

t :: Int -> Dir -> Dir
t n (a, b) = (n * a, n * b)

p :: (Int, Int) -> Dir -> (Int, Int)
p (a, b) (c, d) = (a + c, b + d)

turnRight :: Int -> Dir -> Dir
turnRight 0      d        = d
turnRight (-360) d        = d
turnRight n      (1 , 0 ) = turnRight (n - 90) (0, -1)
turnRight n      (0 , 1 ) = turnRight (n - 90) (1, 0)
turnRight n      (-1, 0 ) = turnRight (n - 90) (0, 1)
turnRight n      (0 , -1) = turnRight (n - 90) (-1, 0)

performMove :: String -> Position -> Position
performMove ('N' : d) p = p { y = y p + read d }
performMove ('S' : d) p = p { y = y p - read d }
performMove ('E' : d) p = p { x = x p + read d }
performMove ('W' : d) p = p { x = x p - read d }
performMove ('L' : d) p = p { dir = turnRight (360 - read d) $ dir p }
performMove ('R' : d) p = p { dir = turnRight (read d) $ dir p }
performMove ('F' : d) ps =
  let d'       = read d
      (x', y') = (x ps, y ps) `p` (d' `t` dir ps)
  in  ps { x = x', y = y' }

moveWpRight :: Int -> (Int, Int) -> (Int, Int)
moveWpRight n wp@(wx, wy) | n `elem` [0, -360] = wp
                          | otherwise          = moveWpRight (n - 90) (wy, -wx)

moveWaypoint :: ((Int, Int), (Int, Int)) -> String -> ((Int, Int), (Int, Int))
moveWaypoint ((wx, wy), sp) ('N' : d) = ((wx, wy + read d), sp)
moveWaypoint ((wx, wy), sp) ('S' : d) = ((wx, wy - read d), sp)
moveWaypoint ((wx, wy), sp) ('E' : d) = ((wx + read d, wy), sp)
moveWaypoint ((wx, wy), sp) ('W' : d) = ((wx - read d, wy), sp)
moveWaypoint (wp      , sp) ('L' : d) = (moveWpRight (360 - read d) wp, sp)
moveWaypoint (wp      , sp) ('R' : d) = (moveWpRight (read d) wp, sp)
moveWaypoint (wp      , sp) ('F' : d) = (wp, sp `p` (read d `t` wp))


day12 :: IO ()
day12 = do
  putStrLn "Day12"
  inp <- lines <$> readFile "in12.txt"
  let endPos = foldl' (flip performMove) (Position (1, 0) 0 0) inp
  print $ abs (x endPos) + abs (y endPos)
  let endPos2 = snd $ foldl' moveWaypoint ((10, 1), (0, 0)) inp
  print $ abs (fst endPos2) + abs (snd endPos2)


