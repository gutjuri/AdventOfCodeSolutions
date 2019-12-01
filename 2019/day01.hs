module Main where

main :: IO ()
main = do
  inp <- map read . lines <$> readFile "inputs/input01.txt"
  let sol1 = ch1 inp
      sol2 = ch2 inp
  print sol1
  print sol2

ch1 :: [Int] -> Int
ch1 = sum . map ((subtract 2) .  (`div` 3))

ch2 :: [Int] -> Int
ch2 = sum . map totalFuel
  where 
    f = (subtract 2). (`div` 3)
    totalFuel = sum . takeWhile (>0) . tail . iterate f 
