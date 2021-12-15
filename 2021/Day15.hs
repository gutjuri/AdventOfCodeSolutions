import           Data.Array
import           Data.Char
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.List                      ( foldl' )
import           Data.Bifunctor                 ( Bifunctor(bimap) )


type Point = (Int, Int)
type PQueue = M.Map Int (S.Set Point)

takeOne :: PQueue -> (PQueue, Point, Int)
takeOne q = if S.null newS
  then (M.delete minKey q, p, minKey)
  else (M.insert minKey newS q, p, minKey)
 where
  (minKey, s) = M.findMin q
  p           = S.elemAt 0 s
  newS        = S.delete p s

insertOne :: PQueue -> Point -> Int -> PQueue
insertOne q p w = M.insertWith S.union w (S.singleton p) q

parseInput :: String -> Array Point Int
parseInput str =
  listArray ((0, 0), (edgeLenY - 1, edgeLenX - 1)) $ map digitToInt $ filter
    (/= '\n')
    str
 where
  edgeLenY = length $ lines str
  edgeLenX = length $ head $ lines str

inBounds :: (Point, Point) -> Point -> Bool
inBounds ((minX, minY), (maxX, maxY)) (a, b) =
  a >= minX && a <= maxX && b >= minY && b <= maxY

findPath :: Point -> S.Set Point -> Array Point Int -> PQueue -> Int
findPath target vis arr q
  | nextPoint == target
  = w
  | otherwise
  = findPath target (S.insert nextPoint vis) arr
    $ foldl' (\acc p -> insertOne acc p (w + arr ! p)) q'
    $ filter (\x -> inBounds bs x && not (x `S.member` vis))
    $ neighbours nextPoint
 where
  (q', nextPoint, w) = takeOne q
  bs                 = bounds arr

neighbours :: Point -> [Point]
neighbours (a, b) = [(a + 1, b), (a, b + 1), (a - 1, b), (a, b - 1)]

mkLargeMap :: Array Point Int -> Array Point Int
mkLargeMap arr = array
  ((0, 0), (maxX - 1, maxY - 1))
  [ ((x, y), getVal x y) | x <- [0 .. maxX - 1], y <- [0 .. maxY - 1] ]
 where
  (oX  , oY  ) = bimap (+ 1) (+ 1) $ snd $ bounds arr
  (maxX, maxY) = (oX * 5, oY * 5)
  getVal x y =
    ((arr ! (x `mod` oX, y `mod` oY)) + (x `div` oX + y `div` oY) - 1)
      `mod` 9
      +     1

getMinRisk :: Array Point Int -> Int
getMinRisk arr =
  findPath (snd $ bounds arr) S.empty arr (M.singleton 0 $ S.singleton (0, 0))

main :: IO ()
main = do
  inp <- parseInput <$> readFile "in15.txt"
  print $ getMinRisk inp
  print $ getMinRisk $ mkLargeMap inp
