module Day05 where

import           Data.List
import           Data.Bifunctor

seatId :: String -> Int
seatId =
  uncurry (+) . first (* 8) . bimap decodeBinary decodeBinary . splitAt 7

decodeBinary :: String -> Int
decodeBinary str = go str 0
 where
  go :: String -> Int -> Int
  go []       n = n
  go (x : xs) n = go xs (2 * n + (if x `elem` "FL" then 0 else 1))

findMissing :: [Int] -> Int
findMissing (x : xs) = go x xs
 where
  go n (n' : ns) | n + 1 /= n' = n + 1
                 | otherwise   = go n' ns

day05 :: IO ()
day05 = do
  putStrLn "Day05"
  inp <- lines <$> readFile "in05.txt"
  let seatNrs = map seatId inp
  print $ maximum seatNrs
  print $ findMissing $ sort seatNrs
