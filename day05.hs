-- Day 5
-- assumes the input to be stored in inp5a.txt

import Data.Char(isLower, toLower)

-- Part 1

xor a b = (a && not b) || (not a && b)

cancelsOut x1 x2 = toLower x1 == toLower x2 && xor (isLower x1) (isLower x2) 

sweepOnce (x1:x2:xs) | cancelsOut x1 x2 = sweepOnce xs
                     | otherwise        = x1:sweepOnce (x2:xs)
sweepOnce r = r

untilStable f xs | xs == p = p
                 | otherwise = untilStable f p
    where
        p = f xs

-- Solution 1        
ch1 = do
    contents <- readFile "inp5a.txt"
    return $ length $ untilStable sweepOnce contents
    
-- Part 2

chars = "abcdefghijklmnopqrstuvwxyz"

-- Solution 2
ch2 = do
    contents <- readFile "inp5a.txt"
    let red = untilStable sweepOnce contents
    return $ minimum $ map length $ map (untilStable sweepOnce) $ map (\chr -> filter (\x -> chr /= toLower x) red) chars
