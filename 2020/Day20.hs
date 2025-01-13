module Day20 where

import           Data.List.Split
import           Data.List
import           Data.Bifunctor
import Debug.Trace

data Image = Image {
  edges :: [String],
  nr :: Int
} deriving Show

instance Eq Image where
  a == b = nr a == nr b

parseInput :: String -> [Image]
parseInput =
  map (go . bimap (splitOneOf " :") (tail . lines) . break (== '\n'))
    . endBy "\n\n"
 where
  go (nrstr, img) = Image { nr = read $ nrstr !! 1, edges = getEdges img }
  getEdges img =
    let transposed = transpose img
    in  [head img, head transposed, last img, last transposed]

fits :: Int -> Int -> [Image] -> [Image] -> Maybe [Image]
fits n len sq left
  | len == 1 || len == 0
  = next
  | len == n * n
  = if verify (sq !! (len - n - 1)) (sq !! (len - 2)) (sq !! (len - 1))
    then Just sq
    else Nothing
  | len `mod` n == 1
  = if verifyFirst (sq !! (len - n - 1)) (sq !! (len - 1))
    then next
    else Nothing
  | len <= n
  = if verifyLeft (sq !! (len - 2)) (sq !! (len - 1)) then next else Nothing
  | otherwise
  = if verify (sq !! (len - n - 1)) (sq !! (len - 2)) (sq !! (len - 1))
    then next
    else Nothing
 where
  newSqs = [ nI' : sq | nI <- left, nI' <- allOrientations nI ]
  verifyFirst above l = (edges above) !! 2 == (edges l) !! 0
  verifyLeft before l = (edges before) !! 3 == (edges l) !! 1
  verify above before l = verifyFirst above l && verifyLeft before l
  next = firstJust
    $ map (\nsq@(l : _) -> fits n (len + 1) nsq (delete l left)) newSqs

firstJust :: [Maybe a] -> Maybe a
firstJust (Nothing  : xs) = firstJust xs
firstJust ((Just x) : _ ) = Just x
firstJust _ = Nothing

allOrientations :: Image -> [Image]
allOrientations (Image cs num) = map (\c -> Image c num) $ take 4 (rotate cs) ++ take 4 (rotate [cs!!0, cs!!2, cs!!1, cs!!3] )

rotate :: [a] -> [[a]]
rotate (x:xs) = (x:xs) : rotate (xs ++[x])

getCornerImages :: Int -> [Image] -> [Image]
getCornerImages n is = [is!!0, is!!(n-1), is!!(n*(n-1)), is!!(n*n-1) ]

day20 :: IO ()
day20 = do
  inp <- parseInput <$> readFile "in20.txt"
  let n = (round (sqrt $ fromIntegral (length inp)))
  let Just sol = fits n 0 [] inp
  print $ map nr $ getCornerImages n sol
  print $ allOrientations $ inp!!0
