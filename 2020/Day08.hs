module Day08 where

import           Data.List
import           Data.Maybe
import qualified Data.Array                    as A
import           Data.Array                     ( Array )

data Instr = Noop Int | Acc Int | Jmp Int
  deriving (Eq, Show)

readInt :: String -> Int
readInt ('+' : xs) = read xs
readInt ('-' : xs) = -read xs

parseLine :: String -> Instr
parseLine str | "nop" `isPrefixOf` str = Noop $ readInt $ drop 4 str
              | "acc" `isPrefixOf` str = Acc $ readInt $ drop 4 str
              | "jmp" `isPrefixOf` str = Jmp $ readInt $ drop 4 str

runUntilDoubleInst :: Array Int (Instr, Bool) -> Int -> Int -> Int
runUntilDoubleInst instr ip acc
  | snd (instr A.! ip') = acc
  | otherwise = runUntilDoubleInst
    (instr A.// [(ip, (fst (instr A.! ip), True))])
    ip'
    acc'
 where
  (ip', acc') = case fst $ instr A.! ip of
    Jmp  x -> (ip + x, acc)
    Acc  x -> (ip + 1, acc + x)
    Noop _ -> (ip + 1, acc)

runUntilTermination :: Array Int (Instr, Bool) -> Int -> Int -> Maybe Int
runUntilTermination instr ip acc
  | snd (A.bounds instr) + 1 == ip' = Just acc'
  | snd (instr A.! ip') = Nothing
  | otherwise = runUntilTermination
    (instr A.// [(ip, (fst (instr A.! ip), True))])
    ip'
    acc'
 where
  (ip', acc') = case fst $ instr A.! ip of
    Jmp  x -> (ip + x, acc)
    Acc  x -> (ip + 1, acc + x)
    Noop _ -> (ip + 1, acc)

possibleProgrammes
  :: Array Int (Instr, Bool) -> Int -> [Array Int (Instr, Bool)]
possibleProgrammes instr pos
  | snd (A.bounds instr) + 1 == pos
  = []
  | isAcc $ fst (instr A.! pos)
  = possibleProgrammes instr $ pos + 1
  | otherwise
  = instr
    A.// [(pos, (flipNJ $ fst $ instr A.! pos, False))]
    :    possibleProgrammes instr (pos + 1)
 where
  isAcc (Acc _) = True
  isAcc _       = False
  flipNJ (Jmp  x) = Noop x
  flipNJ (Noop x) = Jmp x

day08 :: IO ()
day08 = do
  putStrLn "Day08"
  inp <- map parseLine . lines <$> readFile "in08.txt"
  let inpArray = A.listArray (1, length inp) $ zip inp $ repeat False
  print $ runUntilDoubleInst inpArray 1 0
  print $ head $ mapMaybe (\i -> runUntilTermination i 1 0) $ possibleProgrammes
    inpArray
    1
