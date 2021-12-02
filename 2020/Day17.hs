module Day17 where

import           Data.Array
import           Data.List
import           Control.Monad

parseInp :: [String] -> Array (Int, Int, Int) Char
parseInp str =
  array ((0, 0, 0), (lenX + 13, lenY + 13, 14))
    $ zip
        [ (x, y, z)
        | z <- [0 .. 14]
        , y <- [0 .. lenY + 13]
        , x <- [0 .. lenX + 13]
        ]
    $ concatMap concat bigArr3D
 where
  (lenX, lenY) = (length $ head str, length str)
  bigArr2D =
    replicate 7 (replicate (14 + lenX) '.')
      ++ map (\a -> replicate 7 '.' ++ a ++ replicate 7 '.') str
      ++ replicate 7 (replicate (14 + lenX) '.')
  emptyPlane = replicate (lenY + 14) (replicate (lenX + 14) '.')
  bigArr3D   = replicate 7 emptyPlane ++ [bigArr2D] ++ replicate 7 emptyPlane

cycleOnce :: Array (Int, Int, Int) Char -> Array (Int, Int, Int) Char
cycleOnce a =
  a
    // [ ((x, y, z), nextState (x, y, z))
       | x <- [1 .. maxX - 1]
       , y <- [1 .. maxY - 1]
       , z <- [1 .. maxZ - 1]
       ]
 where
  (_, (maxX, maxY, maxZ)) = bounds a
  nextState k
    | a
      !      k
      ==     '#'
      &&     activeNeighbourCnt k
      `elem` [2, 3]
      ||     a
      !      k
      ==     '.'
      &&     activeNeighbourCnt k
      ==     3
    = '#'
    | otherwise
    = '.'
  activeNeighbourCnt k@(u, v, w) =
    length $ filter (\coord -> a ! coord == '#') $ delete
      k
      [ (u', v', w')
      | u' <- [u - 1 .. u + 1]
      , v' <- [v - 1 .. v + 1]
      , w' <- [w - 1 .. w + 1]
      ]

countActive :: Array (Int, Int, Int) Char -> Int
countActive = length . filter (== '#') . elems

printCube :: Array (Int, Int, Int) Char -> IO ()
printCube a = forM_ [0 .. maxZ] $ \pl -> do
  putStrLn ("z=" ++ show pl)
  printPlane pl
 where
  printPlane z = forM_ [0 .. maxX] $ \x -> do
    forM_ [0 .. maxY] (\y -> putChar (a ! (x, y, z)))
    putChar '\n'
  (_, (maxX, maxY, maxZ)) = bounds a


parseInp4d :: [String] -> Array (Int, Int, Int, Int) Char
parseInp4d str =
  array
      ((0, 0, 0, 0), (lenX + 13, lenY + 13, 14, 14))
      (zip
        [ (x, y, z, w)
        | w <- [0 .. 14]
        , z <- [0 .. 14]
        , y <- [0 .. lenY + 13]
        , x <- [0 .. lenX + 13]
        ]
        (repeat '.')
      )
    // zip [ (x, y, 7, 7) | y <- [7 .. lenY + 6], x <- [7 .. lenX + 6] ]
           (concat str)
  where (lenX, lenY) = (length $ head str, length str)

cycleOnce4d
  :: Array (Int, Int, Int, Int) Char -> Array (Int, Int, Int, Int) Char
cycleOnce4d a =
  a
    // [ ((x, y, z, w), nextState (x, y, z, w))
       | x <- [1 .. maxX - 1]
       , y <- [1 .. maxY - 1]
       , z <- [1 .. maxZ - 1]
       , w <- [1 .. maxW - 1]
       ]
 where
  (_, (maxX, maxY, maxZ, maxW)) = bounds a
  nextState k
    | a
      !      k
      ==     '#'
      &&     activeNeighbourCnt k
      `elem` [2, 3]
      ||     a
      !      k
      ==     '.'
      &&     activeNeighbourCnt k
      ==     3
    = '#'
    | otherwise
    = '.'
  activeNeighbourCnt k@(u, v, w, m) =
    length $ filter (\coord -> a ! coord == '#') $ delete
      k
      [ (u', v', w', m')
      | u' <- [u - 1 .. u + 1]
      , v' <- [v - 1 .. v + 1]
      , w' <- [w - 1 .. w + 1]
      , m' <- [m - 1 .. m + 1]
      ]

countActive4d :: Array (Int, Int, Int, Int) Char -> Int
countActive4d = length . filter (== '#') . elems

printCube4d :: Array (Int, Int, Int, Int) Char -> IO ()
printCube4d a = forM_ [0 .. maxW] $ \h -> forM_ [0 .. maxZ] $ \pl -> do
  putStrLn ("z=" ++ show pl ++ "w=" ++ show h)
  printPlane pl h
 where
  printPlane z w = forM_ [0 .. maxX] $ \x -> do
    forM_ [0 .. maxY] (\y -> putChar (a ! (x, y, z, w)))
    putChar '\n'
  (_, (maxX, maxY, maxZ, maxW)) = bounds a


day17 :: IO ()
day17 = do
  putStrLn "Day17"
  inp  <- parseInp . lines <$> readFile "in17.txt"
  --printCube inp
  inp2 <- parseInp4d . lines <$> readFile "in17.txt"
  print $ countActive (iterate cycleOnce inp !! 6)
  print $ countActive4d (iterate cycleOnce4d inp2 !! 6)
