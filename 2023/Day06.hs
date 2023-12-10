module Day06 where

import Data.Bifunctor
import Debug.Trace

type Race = (Int, Int)

parseInp :: String -> [Race]
parseInp = uncurry zip . bimap parseLine parseLine . break (=='\n')
    where
        parseLine :: String -> [Int]
        parseLine = map read . drop 1 . words

solveQuadratic :: (Double, Double, Double) -> (Int, Int)
solveQuadratic (_, b, c) = traceShowId (floor $ (-b+rad)/(-2), ceiling $ (-b - rad)/(-2))
    where
        rad = sqrt((b^2) + 4*c)

-- b*(t-b) > r
-- b*(t-b) - r > 0
-- -bb + bt - r > 0

possibleWins :: Race -> Int
possibleWins (time, record) = h-l-1
    where
        (l, h) = solveQuadratic (-1, fromIntegral time, fromIntegral (-record))

task1 :: [Race] -> Int
task1 = product . map possibleWins 

task2 :: [Race] -> Int
task2 = possibleWins . fixKerning
    where
        fixKerning :: [Race] -> Race
        fixKerning = bimap fixNum fixNum . unzip
        fixNum :: [Int] -> Int
        fixNum = read . concatMap show

main :: IO ()
main = do
    inp <- parseInp <$> readFile "2023/in06.txt"
    print $ task1 inp
    print $ task2 inp