module Main where

data Instr = Forward Int | Up Int | Down Int deriving Show

parseInput :: [String] -> [Instr]
parseInput = map (processToken . words)
 where
  processToken ["forward", n] = Forward $ read n
  processToken ["up"     , n] = Up $ read n
  processToken ["down"   , n] = Down $ read n
  processToken _              = error "parse error"

followPosition :: [Instr] -> (Int, Int)
followPosition = go (0, 0)
 where
  go (h, d) ((Forward n) : xs) = go (h + n, d) xs
  go (h, d) ((Up      n) : xs) = go (h, d - n) xs
  go (h, d) ((Down    n) : xs) = go (h, d + n) xs
  go p      []                 = p

followPosition2 :: [Instr] -> (Int, Int)
followPosition2 = go (0, 0, 0)
 where
  go (h, d, a) ((Forward n) : xs) = go (h + n, d + a * n, a) xs
  go (h, d, a) ((Up      n) : xs) = go (h, d, a - n) xs
  go (h, d, a) ((Down    n) : xs) = go (h, d, a + n) xs
  go (h, d, _) []                 = (h, d)

main :: IO ()
main = do
  instr <- parseInput . lines <$> readFile "in02.txt"
  print $ uncurry (*) $ followPosition instr
  print $ uncurry (*) $ followPosition2 instr
