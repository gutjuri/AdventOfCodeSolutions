module Main where
import           Data.List.Split
import           Data.Bifunctor
import qualified Data.Set                      as S
import           Data.List
import           Data.Function                  ( on )

type Point = (Int, Int)

data Instr = FoldX Int | FoldY  Int deriving Show

parseInp :: String -> (S.Set Point, [Instr])
parseInp = bimap readPoints (readInstrs . tail) . break (== "") . lines
 where
  readPoints :: [String] -> S.Set Point
  readPoints = S.fromList . map (bimap read (read . tail) . break (== ','))
  readInstrs :: [String] -> [Instr]
  readInstrs = map (readInstr . last . splitOn " ")
  readInstr ('x' : _ : xs) = FoldX $ read xs
  readInstr ('y' : _ : xs) = FoldY $ read xs

applyFold :: Instr -> S.Set Point -> S.Set Point
applyFold (FoldX v) = S.map (first (ifB v))
applyFold (FoldY v) = S.map (second (ifB v))

ifB :: (Ord p, Num p) => p -> p -> p
ifB val coord | val < coord = coord - 2 * (coord - val)
              | otherwise   = coord

p1 :: Instr -> S.Set Point -> Int
p1 i = S.size . applyFold i

p2 :: [Instr] -> S.Set Point -> String
p2 i s = printField $ foldl' (flip applyFold) s i

printField :: S.Set Point -> String
printField s =
  unlines
    . chunksOf (maxX + 1)
    $ [ (\p -> if S.member p s then '#' else ' ') (x, y)
      | y <- [0 .. maxY]
      , x <- [0 .. maxX]
      ]
 where
  maxX = fst $ maximumBy (compare `on` fst) s
  maxY = snd $ maximumBy (compare `on` snd) s

main :: IO ()
main = do
  (pts, instr) <- parseInp <$> readFile "in13.txt"
  print $ p1 (head instr) pts
  putStrLn $ p2 instr pts
