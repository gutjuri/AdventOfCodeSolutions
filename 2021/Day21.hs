module Main where
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.Char                      ( digitToInt )
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M

parseInp :: String -> (Int, Int)
parseInp = bimap f f . break (== '\n')
 where
  f :: String -> Int
  f = digitToInt . last

playRound :: Int -> Int -> Int -> Int -> Int -> Int
playRound i pos1 pos2 score1 score2
  | score1 >= 1000 || score2 >= 1000
  = (i - 1) * min score1 score2
  | otherwise
  = let newF = ((pos1 + f i) - 1) `mod` 10 + 1
    in  playRound (i + 3) pos2 newF score2 $ score1 + newF
 where
  f x =
    ((x - 1) `mod` 1000) + 1 + (x `mod` 1000) + 1 + ((x + 1) `mod` 1000) + 1


type DP = M.Map (Int, Int, Int, Int, Int) (Integer, Integer)

rollCombs :: M.Map Int Integer
rollCombs = M.fromListWith (+) $ map
  (\(x, y, z) -> (x + y + z, 1))
  (do
    r1 <- [1 .. 3]
    r2 <- [1 .. 3]
    r3 <- [1 .. 3]
    return (r1, r2, r3)
  )

play :: DP -> Int -> Int -> Int -> Int -> Int -> (Integer, Integer, DP)
play dp turn pos1 pos2 score1 score2
  | score1 >= 21
  = (1, 0, dp)
  | score2 >= 21
  = (0, 1, dp)
  | otherwise
  = let (n1', n2', dp'') =
          foldl'
              (\(n1, n2, dp') (st, p) ->
                (\(p1w, p2w, dpx) -> (p1w * p + n1, p2w * p + n2, dpx)) $ play
                  dp'
                  (1 - turn)
                  (calcPos (1 - turn) pos1 st)
                  (calcPos turn pos2 st)
                  (calcPos (1 - turn) pos1 st * (1 - turn) + score1)
                  (calcPos turn pos2 st * turn + score2)
              )
              (0, 0, dp)
            $ M.assocs rollCombs
    in  case M.lookup (turn, pos1, pos2, score1, score2) dp of
          Just (x, y) -> (x, y, dp)
          Nothing ->
            ( n1'
            , n2'
            , M.insert (turn, pos1, pos2, score1, score2) (n1', n2') dp''
            )
  where calcPos t pos n = ((pos - 1) + (t * n)) `mod` 10 + 1

maxPlayerWins :: Ord a => (a, a, c) -> a
maxPlayerWins (a, b, _) = max a b

main :: IO ()
main = do
  (s1, s2) <- parseInp <$> readFile "in21.txt"
  print $ playRound 1 s1 s2 0 0
  print $ maxPlayerWins $ play M.empty 0 s1 s2 0 0
