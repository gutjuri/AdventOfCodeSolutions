module Main where

import           Data.Array
import           Data.Bifunctor
import           Data.Complex                   ( imagPart )
import           Data.List                      ( foldl' )
import           Data.List.Split
import qualified Data.Map.Strict               as M

type Rules = Array Int Bool

type Image = M.Map (Int, Int) Bool

bound :: Int
bound = 200

parseInp :: String -> (Rules, Image)
parseInp str = bimap parseRules parseImage $ (\[x, y] -> (x, y)) $ splitOn
  "\n\n"
  str
 where
  l = round . sqrt . fromIntegral . length
  parseRules :: String -> Rules
  parseRules = listArray (0, 511) . map (== '#')
  parseImage :: String -> Image
  parseImage = toMap . concat . lines
  toMap s =
    M.fromList
        (zipWith (curry (second (== '#')))
                 ([ (x, y) | x <- [0 .. (l s - 1)], y <- [0 .. (l s - 1)] ])
                 s
        )
      `M.union` M.fromList
                  [ ((x, y), False)
                  | x <- [-bound .. bound]
                  , y <- [-bound .. bound]
                  ]

getNeighbourCoords :: (Int, Int) -> [(Int, Int)]
getNeighbourCoords (x, y) =
  [ (x - 1, y - 1)
  , (x - 1, y)
  , (x - 1, y + 1)
  , (x    , y - 1)
  , (x    , y)
  , (x    , y + 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  ]

getRulesIndex :: Image -> (Int, Int) -> Bool -> Int
getRulesIndex img c bg =
  toInt $ map (\x -> M.findWithDefault bg x img) $ getNeighbourCoords c

printMap :: Image -> String
printMap = go 0 . M.assocs
 where
  go :: Int -> [((Int, Int), Bool)] -> String
  go x l@(((i, _), b) : xs) | x < i     = '\n' : go i l
                            | otherwise = (if b then '#' else '.') : go i xs
  go _ [] = []

toInt :: [Bool] -> Int
toInt = foldl' (\acc x -> acc * 2 + if x then 1 else 0) 0

getPixelValue :: Rules -> Image -> (Int, Int) -> Bool -> Bool
getPixelValue r img c bg = r ! getRulesIndex img c bg

step :: Rules -> Image -> Image
step r img =
  M.mapWithKey (\c _ -> getPixelValue r img c (img M.! (bound, bound))) img

countLitUp :: Image -> Int
countLitUp =
  sum
    . map ((\x -> if x then 1 else 0) . snd)
    . filter
        (\((x, y), _) ->
          x > -bound + 5 && x < bound - 5 && y > -bound + 5 && y < bound - 5
        )
    . M.assocs

p1 :: Rules -> Image -> Int
p1 r img = countLitUp $ step r $ step r img

p2 :: Rules -> Image -> Int
p2 r = countLitUp . (!! 50) . iterate (step r)

main :: IO ()
main = do
  (r, img) <- parseInp <$> readFile "in20.txt"
  print $ p1 r img
  print $ p2 r img
