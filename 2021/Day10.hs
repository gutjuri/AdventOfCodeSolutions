module Main where
import           Data.List                      ( foldl'
                                                , sort
                                                )

data ParseResult = Ok | Corrupted Char | Incomplete String deriving Show

getClosingComponent :: Char -> Char
getClosingComponent x | x == '('  = ')'
                      | x == '{'  = '}'
                      | x == '['  = ']'
                      | otherwise = '>'

getWorth :: ParseResult -> Int
getWorth (Corrupted x) | x == ')'  = 3
                       | x == '}'  = 1197
                       | x == ']'  = 57
                       | otherwise = 25137
getWorth _ = 0

getWorth2 :: Char -> Int
getWorth2 x | x == ')'  = 1
            | x == '}'  = 3
            | x == ']'  = 2
            | otherwise = 4

isOpening :: Char -> Bool
isOpening = (`elem` "<{[(")

getCorrupted :: String -> String -> ParseResult
getCorrupted [] (x : xs) | isOpening x = getCorrupted [x] xs
                         | otherwise   = Corrupted x
getCorrupted stack@(f : fs) (x : xs)
  | isOpening x = getCorrupted (x : stack) xs
  | otherwise = if x == getClosingComponent f
    then getCorrupted fs xs
    else Corrupted x
getCorrupted stack@(_ : _) [] = Incomplete $ map getClosingComponent stack
getCorrupted []            [] = Ok

p1 :: [String] -> Int
p1 = sum . map (getWorth . getCorrupted [])

p2 :: [String] -> Int
p2 =
  getMiddleScore
    . map (\(Incomplete x) -> getScore x)
    . filter isIncomplete
    . map (getCorrupted [])
 where
  isIncomplete (Incomplete _) = True
  isIncomplete _              = False
  getScore = foldl' (\acc x -> acc * 5 + getWorth2 x) 0

getMiddleScore :: [Int] -> Int
getMiddleScore l = sort l !! (length l `div` 2)

main :: IO ()
main = do
  inp <- words <$> readFile "in10.txt"
  print $ p1 inp
  print $ p2 inp
