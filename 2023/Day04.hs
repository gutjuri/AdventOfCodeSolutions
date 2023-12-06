module Day04 where

import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Card = Card Int (S.Set Int) (S.Set Int)
  deriving (Show)

parseLine :: String -> Card
parseLine str = Card cid winners ownNums
  where
    (cid, r) = (\[cidstr, rs] -> (read $ drop 5 cidstr, rs)) $ splitOn ": " str
    [winners, ownNums] = map (S.fromList . map read . words) $ splitOn " | " r

myWinningNumbers :: Card -> S.Set Int
myWinningNumbers (Card _ w o) = S.intersection w o

worth :: Int -> Int
worth 0 = 0
worth n = 2 ^ (n - 1)

task1 :: [Card] -> Int
task1 = sum . map (worth . S.size . myWinningNumbers)

getInitialCards :: [Card] -> M.Map Int Int
getInitialCards = M.fromList . flip zip (repeat 1) . map (\(Card cid _ _) -> cid)

task2 :: [Card] -> Int
task2 cs = sum $ M.elems $ go (getInitialCards cs) cs
  where
    go :: M.Map Int Int -> [Card] -> M.Map Int Int
    go acc [] = acc
    go acc (c@(Card cid _ _) : cs') = go (M.unionWith (+) acc $ M.fromList (zip (map (\(Card cid _ _) -> cid) $ take (S.size $ myWinningNumbers c) cs') (repeat (acc M.! cid)))) cs'

main :: IO ()
main = do
  inp <- map parseLine . lines <$> readFile "2023/in04.txt"
  print $ task1 inp
  print $ task2 inp