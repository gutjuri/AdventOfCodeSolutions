-- Day 1

-- a)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

findSeqSum = scanl (\acc x -> acc + readToInt x) 0
    where
        readToInt str | head str == '+' = (read $ tail str)
                      | otherwise       = -(read $ tail str)
        
ch1 = do
    let file = "inp1a.txt"
    contents <- readFile file
    return $ last $ findSeqSum (splitOn "\n" contents)

-- b)
dup xs = dup' xs Set.empty
  where dup' [] _     = error "Never happens ;)"
        dup' (x:xs) s | Set.member x s = x
                      | otherwise      = dup' xs (Set.insert x s)
                       
ch2 = do
    let file = "inp1a.txt"
    contents <- readFile file
    return $ dup $ findSeqSum $ cycle (splitOn "\n" contents)
