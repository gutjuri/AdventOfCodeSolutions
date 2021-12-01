module Day17 where

import Data.Array
import Data.List
import Control.Monad

parseInp :: [String] -> Array (Int, Int, Int) Char
parseInp str = array ((0, 0, 0), (lenX + 13, lenY + 13, 14))
              $ zip [(x, y, z) | z <- [0..14], y <- [0..lenY+13], x <- [0..lenX+13]]
              $ concatMap concat bigArr3D
  where
    (lenX, lenY) = (length $ head str, length str)
    bigArr2D = replicate 7 (replicate (14+lenX) '.') 
             ++ map (\a -> replicate 7 '.' ++ a ++ replicate 7 '.') str
             ++ replicate 7 (replicate (14+lenX) '.')
    emptyPlane = replicate (lenY+14) (replicate (lenX+14) '.')
    bigArr3D = replicate 7 emptyPlane ++ [bigArr2D] ++ replicate 7 emptyPlane

cycleOnce :: Array (Int, Int, Int) Char -> Array (Int, Int, Int) Char
cycleOnce a = a // [ ((x, y, z), nextState (x, y, z)) | x <- [1..maxX-1], y <- [1..maxY-1], z <- [1..maxZ-1]]
  where
    (_, (maxX, maxY, maxZ)) = bounds a
    nextState k | a!k == '#' && activeNeighbourCnt k `elem` [2, 3] 
                  || a!k == '.' && activeNeighbourCnt k == 3 = '#'
                | otherwise  = '.'
    activeNeighbourCnt k@(u, v, w) = length $ filter (\coord -> a!coord == '#')
                            $ delete k [(u', v', w') | u' <- [u-1..u+1], v' <- [v-1..v+1], w' <- [w-1..w+1]]

countActive :: Array (Int, Int, Int) Char -> Int 
countActive = length . filter (=='#') . elems

printCube :: Array (Int, Int, Int) Char -> IO ()
printCube a = forM_ [0..maxZ] $ \pl -> do 
                putStrLn ("z=" ++ show pl)
                printPlane pl
 where
    printPlane z = forM_ [0..maxX] $ \x -> do
                    forM_ [0..maxY] (\y -> putChar (a!(x, y, z)))
                    putChar '\n'
    (_, (maxX, maxY, maxZ)) = bounds a

day17 :: IO ()
day17 = do
  putStrLn "Day17"
  inp <- parseInp . lines <$> readFile "in17.txt"
  --printCube inp
  print $ countActive (iterate cycleOnce inp !! 6)