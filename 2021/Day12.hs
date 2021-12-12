module Main where

import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.List                      ( foldl'
                                                , delete
                                                )
import           Data.Char

parseInput :: String -> M.Map String [String]
parseInput =
  foldl'
      (\acc [x, y] -> M.insertWith (++) y [x] $ M.insertWith (++) x [y] acc)
      M.empty
    . map words
    . lines
    . map (\c -> if c == '-' then ' ' else c)

isSmallCave :: String -> Bool
isSmallCave = isLower . head

getPaths :: Bool -> M.Map String [String] -> Int
getPaths l = go l S.empty "start" . adjustAll (delete "start")
 where
  go tw vis "end" m = 1
  go tw vis cn m
    | tw && isSmallCave cn && cn `S.member` vis = 0
    | tw && isSmallCave cn = sum
    $ map (\n -> go tw (S.insert cn vis) n m) (m M.! cn)
    | isSmallCave cn && cn `S.member` vis = sum
    $ map (\n -> go True (S.insert cn vis) n m) (m M.! cn)
    | isSmallCave cn = sum
    $ map (\n -> go False (S.insert cn vis) n m) (m M.! cn)
    | otherwise = sum $ map (\n -> go tw vis n m) (m M.! cn)

adjustAll :: Ord k => (a -> a) -> M.Map k a -> M.Map k a
adjustAll f m =
  foldl' (\acc x -> M.insert x (f $ m M.! x) acc) M.empty $ M.keys m

main :: IO ()
main = do
  inp <- parseInput <$> readFile "in12.txt"
  print $ getPaths True inp
  print $ getPaths False inp
