module Day14 where

import           Data.List
import           Data.Bits
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as M

data Mask = Mask Int Int
  deriving Show

data Command = SetMask Mask| Assignment Int Int
  deriving Show

readBinary :: String -> Int
readBinary = foldl' (\acc x -> 2 * acc + (if x == '1' then 1 else 0)) 0

parseLn :: String -> Command
parseLn str
  | "mask" `isPrefixOf` str
  = let pl = drop 7 str
    in  SetMask $ Mask
          (readBinary $ map (\x -> if x == 'X' then '0' else '1') pl)
          (readBinary pl)
  | otherwise
  = let addr = read $ takeWhile (/= ']') $ drop 4 str
        val  = read $ drop 2 $ dropWhile (/= '=') str
    in  Assignment addr val

applyMask :: Mask -> Int -> Int
applyMask (Mask m1 m2) i = (m1 .&. m2) .|. (complement m1 .&. i)

nextStep :: Mask -> Command -> IntMap Int -> (Mask, IntMap Int)
nextStep _ (SetMask m         ) im = (m, im)
nextStep m (Assignment pos val) im = (m, M.insert pos (applyMask m val) im)

nextStepV2 :: Mask -> Command -> IntMap Int -> (Mask, IntMap Int)
nextStepV2 _ (SetMask m         ) im = (m, im)
nextStepV2 m (Assignment pos val) im = (m, insertAll im (allAddr m pos 35) val)

allAddr :: Mask -> Int -> Int -> [Int]
allAddr _ pos (-1) = [pos]
allAddr m'@(Mask m1 m2) pos p
  | m1 `testBit` p = allAddr m' (pos .|. (m2 .&. bit p)) (p - 1)
  | otherwise =  allAddr m' (pos `setBit` p)   (p - 1)
  ++ allAddr m' (pos `clearBit` p) (p - 1)
insertAll :: IntMap Int -> [Int] -> Int -> IntMap Int
insertAll m' ks v = (M.fromList $ zip ks $ repeat v) `M.union` m'

day14 :: IO ()
day14 = do
  putStrLn "Day14"
  inp <- map parseLn . lines <$> readFile "in14.txt"
  let sol1 = foldl' (+) 0 $ snd $ foldl' (\(m, im) c -> nextStep m c im)
                                         (Mask 0 0, M.empty)
                                         inp
  print sol1
  let sol2 = foldl' (+) 0 $ snd $ foldl' (\(m, im) c -> nextStepV2 m c im)
                                         (Mask 0 0, M.empty)
                                         inp
  print sol2
