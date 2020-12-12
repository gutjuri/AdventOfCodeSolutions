module Day12 where

import Data.List

type  Dir = (Int, Int)

data Position = Position {
  dir :: Dir,
  x :: Int,
  y :: Int
}

t :: Int -> Dir -> Dir
t n (a, b) = (n * a, n * b)

p :: (Int, Int) -> Dir -> (Int, Int)
p (a, b) (c, d) = (a+c, b+d)

turnRight :: Int -> Dir -> Dir
turnRight 0 d = d
turnRight (-360) d = d
turnRight n (1, 0) = turnRight (n-90) (0, 1)
turnRight n (0, 1) = turnRight (n-90) (-1, 0)
turnRight n (-1, 0) = turnRight (n-90) (0, -1)
turnRight n (0, -1) = turnRight (n-90) (1, 0)
 

performMove :: String -> Position -> Position
performMove ('N':d) p = p { x = x p - read d}
performMove ('S':d) p = p { x = x p + read d}
performMove ('E':d) p = p { x = y p + read d}
performMove ('W':d) p = p { x = y p - read d}
performMove ('L':d) p = p { dir = turnRight (360 - read d) $ dir p}
performMove ('R':d) p = p { dir = turnRight (read d) $ dir p}
performMove ('F':d) ps = let d' = read d
                             (x', y') = (x ps, y ps) `p` (d' `t` dir ps)
                         in ps { x = x', y = y' }

day12 :: IO ()
day12 = do
  putStrLn "Day12"
  inp <- lines <$> readFile "day12.txt"
  let endPos = foldl' (flip performMove) (Position (1, 0) 0 0) inp
  print $ x endPos + y endPos


