-- Day 1
-- Assumes that the input file is saved as "inp1a.txt". 

-- a)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

findSeqSum = scanl (+) 0 . map read . map (filter (\c -> c /= '+'))

-- First part
ch1 = do
    let file = "inp1a.txt"
    contents <- readFile file
    return $ last $ findSeqSum (splitOn "\n" contents)

-- b)
dup = dup' Set.empty
  where dup' s (x:xs) | Set.member x s = x
                      | otherwise      = dup' (Set.insert x s) xs

-- Second part
ch2 = do
    let file = "inp1a.txt"
    contents <- readFile file
    return $ dup $ findSeqSum $ cycle (splitOn "\n" contents)
    
