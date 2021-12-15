module Main where

import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Char                      ( digitToInt )
import           Data.List
import           Data.Bifunctor                 ( Bifunctor(second) )


type Field = M.Map (Int, Int) Int

parseInp :: String -> Field
parseInp str = M.fromList $ zip [ (x, y) | y <- [0 .. 9], x <- [0 .. 9] ]
                                (map digitToInt $ concat $ lines str)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours =
  filter (\(x, y) -> x >= 0 && x <= 9 && y >= 0 && y <= 9)
    . zipWith
        plus
        [(0, 1), (1, 0), (1, 1), (-1, 0), (0, -1), (-1, -1), (1, -1), (-1, 1)]
    . repeat
  where plus (x, y) (a, b) = (x + a, y + b)

step :: Field -> (Int, Field)
step = second (M.map (\x -> if x > 9 then 0 else x)) . go S.empty . M.map (+ 1)
 where
  go :: S.Set (Int, Int) -> Field -> (Int, Field)
  go flashed field =
    let
      flashing = M.filterWithKey (\k _ -> not $ S.member k flashed)
        $ M.filter (> 9) field
      flashNeighbours = concatMap neighbours $ M.keys flashing
      newField        = foldl' (flip (M.adjust (+ 1))) field flashNeighbours
      newFlashed      = flashed `S.union` S.fromAscList (M.keys flashing)
      (n, nf) =
        if M.null flashing then (0, newField) else go newFlashed newField
    in
      (M.size flashing + n, nf)

p1 :: Field -> Int
p1 = go 0 0
 where
  go 100 x _ = x
  go n   x f = let (x', nf) = step f in go (n + 1) (x + x') nf

p2 :: Field -> Int
p2 = go 0
 where
  go x f | all (== 0) f = x
         | otherwise    = go (x + 1) $ snd $ step f

main :: IO ()
main = do
  inp <- parseInp <$> readFile "in11.txt"
  print $ p1 inp
  print $ p2 inp
