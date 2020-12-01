import qualified Data.Set                      as S
import           Data.List
import           Data.Maybe

has2020Sum :: Int -> S.Set Int -> Either (S.Set Int) Int
has2020Sum n st | S.member (2020 - n) st = Right $ n * (2020 - n)
                | otherwise              = Left $ S.insert n st

ch1 :: [Int] -> S.Set Int -> IO ()
ch1 (n : ns) st = case has2020Sum n st of
  Left  st' -> ch1 ns st'
  Right n'  -> print n'

triplets :: [Int] -> [[Int]]
triplets xs = do
  a <- xs
  b <- xs
  c <- xs
  return [a, b, c]

ch2 :: [Int] -> IO ()
ch2 = print . product . fromJust . find ((== 2020) . sum) . triplets

main :: IO ()
main = do
  ns <- map read . lines <$> getContents
  ch1 ns S.empty
  ch2 ns
