module Main where
import           Data.List.Split                ( splitOn )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.List

type Entry = ([String], [String])

parseInput :: String -> [Entry]
parseInput = map ((\[a, b] -> (a, b)) . map words . splitOn "|") . lines

p1 :: [Entry] -> Int
p1 = sum . map (length . filter ((`elem` [2, 3, 4, 7]) . length) . snd)

p2 :: [Entry] -> Int
p2 = sum . map decodeEntry

digits :: M.Map String Int
digits = M.fromList $ zip
  [ "abcefg"
  , "cf"
  , "acdeg"
  , "acdfg"
  , "bcdf"
  , "abdfg"
  , "abdefg"
  , "acf"
  , "abcdefg"
  , "abcdfg"
  ]
  [0 ..]

possibleMappings :: [M.Map Char Char]
possibleMappings = do
  a' <- ls
  b' <- ls \\ [a']
  c' <- ls \\ [a', b']
  d' <- ls \\ [a', b', c']
  e' <- ls \\ [a', b', c', d']
  f' <- ls \\ [a', b', c', d', e']
  g' <- ls \\ [a', b', c', d', e', f']
  return $ M.fromList $ zip [a', b', c', d', e', f', g'] ls
  where ls = "abcdefg"

applyMapping :: M.Map Char Char -> String -> String
applyMapping m = map (m M.!)

digitSet :: S.Set String
digitSet = S.fromList $ M.keys digits

decodeEntry :: Entry -> Int
decodeEntry (wires, outp) = case correctMapping of
  Just f  -> toInt 0 $ map ((digits M.!) . (sort . applyMapping f)) outp
  Nothing -> error "impossible"
 where
  mappingsApplied :: [([String], M.Map Char Char)]
  mappingsApplied =
    map (\f -> (map (sort . applyMapping f) wires, f)) possibleMappings
  correctMapping :: Maybe (M.Map Char Char)
  correctMapping =
    snd <$> find (\(ws, _) -> S.fromList ws == digitSet) mappingsApplied
  toInt acc []       = acc
  toInt acc (x : xs) = toInt (acc * 10 + x) xs

main :: IO ()
main = do
  inp <- parseInput <$> readFile "in08.txt"
  print $ p1 inp
  print $ p2 inp
