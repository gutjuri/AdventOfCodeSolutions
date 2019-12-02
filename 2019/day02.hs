module Main where

import qualified Data.Map.Strict as M

type Programme = M.Map Int Int

readProgramme :: IO Programme
readProgramme = M.fromList . zip [0..] . map read . words . map (\c -> if c == ',' then ' ' else c) <$> readFile "inputs/input02.txt"

runProg :: Int -> Programme -> Programme
runProg pc prog | prog M.! pc == 99 = prog
                | prog M.! pc == 1  = runOp (+) pc prog
                | prog M.! pc == 2  = runOp (*) pc prog
                | otherwise         = error $ show pc
  where
    runOp :: (Int -> Int -> Int) -> Int -> Programme -> Programme
    runOp op pc prog = runProg (pc + 4) $ M.insert posC ((prog M.! posA) `op` (prog M.! posB)) prog
    posA = prog M.! (pc + 1)
    posB = prog M.! (pc + 2)
    posC = prog M.! (pc + 3)

progOutput :: Int -> Int -> Programme -> Int
progOutput noun verb prog = (runProg 0 prog') M.! 0
  where 
    prog' = M.insert 1 noun $ M.insert 2 verb prog
    
main :: IO ()
main = do
  prog <- readProgramme
  print $ progOutput 12 2 prog
  let candidates = [(a, b) | a <- [0..99], b <- [0..99]]
      target     = 19690720
  print $ head [100 * a + b | (a, b) <- candidates, progOutput a b prog == target]
