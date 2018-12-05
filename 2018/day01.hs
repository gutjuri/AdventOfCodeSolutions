-- Day 1
-- Assumes that the input file is saved as "inp1a.txt". 

-- a)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

findSeqSum xs = scanl (+) 0 $ map read $ map (filter (\c -> c /= '+')) xs

-- First part
ch1 = do
    let file = "inp1a.txt"
    contents <- readFile file
    return $ last $ findSeqSum (splitOn "\n" contents)

-- b)
dup xs = dup' xs Set.empty
  where dup' [] _     = error "Never happens ;)"
        dup' (x:xs) s | Set.member x s = x
                      | otherwise      = dup' xs (Set.insert x s)

-- Second part
ch2 = do
    let file = "inp1a.txt"
    contents <- readFile file
    return $ dup $ findSeqSum $ cycle (splitOn "\n" contents)
