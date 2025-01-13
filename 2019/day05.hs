module Main where

import qualified Data.Map.Strict as M

type Programme = M.Map Int Int

readProgramme :: IO Programme
readProgramme = M.fromList . zip [0..] . map read . words . map (\c -> if c == ',' then ' ' else c) <$> readFile "inputs/input05.txt"

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
    runInp = runProg (pc + 2)

