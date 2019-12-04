{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Bifunctor      (bimap)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as M
import           Data.Map.Strict     (Map, lookup, fromList)
import qualified Data.Set            as S
import           Data.List           (foldl')
import           Data.Maybe          (catMaybes)
import           Prelude hiding      (lookup)

data Dir = U | R | D | L
  deriving (Read, Show)

type Instr = (Dir, Int)
type Point = (Int, Int)

d :: Dir -> Point
d U = (0, 1)
d R = (1, 0)
d D = (0, -1)
d L = (-1, 0)


p :: Point -> Point -> Point
(x, y) `p` (x', y') = (x+x', y+y')

dist :: Point -> Int
dist (x, y) = abs x + abs y

readInstr :: IO ([Instr], [Instr])
readInstr = (\[l1, l2] -> (l1, l2)) .  map processLine . lines <$> readFile "inputs/input03.txt"
  where
    processLine :: String -> [Instr]
    processLine = map parseInstr . words . map (\case {',' -> ' '; x -> x}) 
    parseInstr :: String -> Instr
    parseInstr (d:r) = bimap read read (d:[], r)

mkPoints' :: [Instr] -> IntMap Point
mkPoints' = fst . foldl' (\(m, (c, l)) (dir, n) -> let nm = f m c l n (d dir) in (nm, (nm M.! (n+l), (n+l)))) (M.empty, ((0, 0), 0))
  where
    f map _ _ 0 _     = map
    f map cp cn n dir = f (M.insert (cn+1) (cp `p` dir) map) (cp `p` dir) (cn+1) (n-1) dir

antimap :: IntMap Point -> Map Point Int
antimap = fromList . map (\(a, b) -> (b,a)) . M.assocs

main :: IO ()
main = do
  i <- readInstr
  let (rec1, rec2) = bimap mkPoints' mkPoints' i
      solA = S.findMin . S.map dist $ S.intersection (S.fromList $ M.elems rec1)  (S.fromList $ M.elems rec2)
      cer2 = antimap rec2
      solB = minimum $ catMaybes $ map (\(d, p) -> (+d) <$> lookup p cer2) $ M.assocs rec1
  print $ solA
  print $ solB

