module Day02 where

import Data.List.Split
import Data.Bifunctor
import Debug.Trace

--           R    G    B
type Draw = (Int, Int, Int)

data Game = Game Int [Draw]
    deriving Show

parseLine :: String -> Game
parseLine str = Game id draws
    where
        (id, rs) = bimap (read . drop 5) (drop 2) $ break (==':') str
        draws = map parseDraw $ splitOn "; " rs

parseDraw :: String -> Draw
parseDraw = foldl f (0, 0, 0) . splitOn ", "
    where
        f acc str = newDraw acc $ bimap read (!!1) $ break (==' ') str
        newDraw (cr, cg, cb) (n, 'r') = (n, cg, cb)
        newDraw (cr, cg, cb) (n, 'g') = (cr, n, cb)
        newDraw (cr, cg, cb) (n, 'b') = (cr, cg, n)
        newDraw _ (_, x) = error (x:"")

constrainTask1 :: Draw
constrainTask1 = (12, 13, 14) 

fulfilConstraint :: Draw -> Game -> Bool
fulfilConstraint (mr, mg, mb) (Game _ ds) = all (\(r, g, b) -> r <= mr && g <= mg && b <= mb) ds

task1 :: [Game] -> Int
task1 = sum . map (\(Game id _) -> id) . filter (fulfilConstraint constrainTask1)

getPowerMinSet :: Game -> Int
getPowerMinSet (Game _ ds) = (\(a, b, c) -> a*b*c) $ fmapTuple maximum $ unzip3 ds
    where fmapTuple f (a,b,c) = (f a, f b, f c)

task2 :: [Game] -> Int
task2 = sum . map getPowerMinSet

main = do
    inp <- map parseLine <$> lines <$> readFile "2023/in02.txt"
    print $ task1 inp
    print $ task2 inp