import Data.Set (Set)
import qualified Data.Set as S

type Pos = (Int, Int)

plus :: Pos -> Pos -> Pos
plus (xa, ya) (xb, yb) = (xa+xb, ya+yb)

first [] = []
first (x:xs) = x:second xs

second [] = []
second (x:xs) = first xs

stepThrough :: String -> Pos -> Set Pos
stepThrough [] p = S.singleton p
stepThrough (d:ds) cPos = S.insert cPos $ stepThrough ds $ (dir d) `plus` cPos
  where 
    dir '^' = (0, 1)
    dir '>' = (1, 0)
    dir 'v' = (0, -1)
    dir _   = (-1, 0)

main :: IO ()
main = do
  inp <- readFile "input/in3.txt"
  putStrLn $ show $ S.size $ stepThrough inp (0, 0)
  let visSanta = stepThrough (first inp) (0, 0)
  let visRobot = stepThrough (second inp) (0, 0)
  putStrLn $ show $ S.size $ S.union visSanta visRobot