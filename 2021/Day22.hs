module Main where
import           Data.Char                      ( isDigit )
import           Data.List
import           Data.List.Split                ( splitOn )
import qualified Data.Set                      as S

type Range = (Int, Int, Int, Int, Int, Int)
type Point3 = (Int, Int, Int)

bound :: Int
bound = 50

parseInp :: String -> [(Bool, Range)]
parseInp = map ((\[m, r] -> (m == "on", parseRange r)) . words) . lines
 where
  parseRange =
    (\[a, b, c, d, e, f] -> (a, b, c, d, e, f)) . map read . words . map
      (\c -> if isDigit c || c == '-' then c else ' ')

getInRange :: Bool -> Range -> S.Set Point3
getInRange b (x1, x2, y1, y2, z1, z2) = S.fromAscList
  [ (x, y, z)
  | x <- [x1 .. x2]
  , b || (x <= bound && x >= -bound)
  , y <- [y1 .. y2]
  , b || (y <= bound && y >= -bound)
  , z <- [z1 .. z2]
  , b || (z <= bound && z >= -bound)
  ]

doSteps :: Bool -> [(Bool, Range)] -> S.Set Point3
doSteps b = foldl' f S.empty
 where
  f acc (True , range) = acc `S.union` getInRange b range
  f acc (False, range) = acc S.\\ getInRange b range

countOn :: S.Set Point3 -> Int
countOn = S.size

p1 :: [(Bool, Range)] -> Int
p1 = countOn . doSteps False

p2 :: [(Bool, Range)] -> Int
p2 instr = countOn $ doSteps' $ breakInp bps instr
    where
        bps = getBreakPts instr
        countOn = S.foldl' (\acc (a,b,c,d,e,f) -> acc + (b-a+1) * (d-c+1) * (f-e+1)) 0

doSteps' :: [(Bool, Range)] -> S.Set Range
doSteps' = foldl' f S.empty 
    where
        f acc (True, range) = S.insert range acc
        f acc (False, range) = S.filter (/=range) acc
        inRange (a,b,c,d,e,f) (a', b', c', d', e', f') = a >= a' && b <= b' && c >= c' && d <= d' && e >= e' && f <= f'

getBreakPts :: [(Bool, Range)] -> ([Int], [Int], [Int])
getBreakPts = trimap sort . foldl' f ([], [], [])
    where
        f (xs, ys, zs) (_, (a,b,c,d,e,f)) = (a:b:xs, c:d:ys, e:f:zs)
        trimap f (a, b, c) = (f a, f b, f c)

breakInp :: ([Int], [Int], [Int]) -> [(Bool, Range)] -> [(Bool, Range)]
breakInp (xbps, ybps, zbps) = concatMap breakUp
 where
     breakUp (x, (a, b, c, d, e, f)) = combs x (breaks xbps a b) (breaks ybps c d) (breaks zbps e f)
     breaks bs n1 n2 = mkPairs $ (n1:) $ (++[n2]) $ takeWhile (<n2) $ dropWhile (<= n1) bs
     mkPairs [_] = []
     mkPairs (x:y:xs) = (x, y) : mkPairs (y:xs) 
     combs x b1 b2 b3= do
         (a, b) <- b1
         (c, d) <- b2
         (e, f) <- b3
         return (x, (a,b,c,d,e,f))

    

main :: IO ()
main = do
  inp <- parseInp <$> readFile "in22.txt"
  print $ p1 inp
  print $ p2 inp
