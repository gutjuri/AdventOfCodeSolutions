import Data.List

parseInput :: String -> [[String]]
parseInput = groupBy (\a b -> null a == null b) . lines

p1 :: [[String]] -> Int
p1 = sum . map (length . foldl1 union)

p2 :: [[String]] -> Int
p2 = sum . map (length . foldl1 intersect)

main :: IO ()
main = do
  inp <- parseInput <$> getContents
  print $ p1 inp
  print $ p2 inp