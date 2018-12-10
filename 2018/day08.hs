-- Day 8

import Data.List.Split (splitOn)
import Data.List (sort)

data Tree = Node [Tree] [Int] deriving(Show)

parse :: String -> [Int]
parse = map read . splitOn " "

readTree :: [Int] -> (Tree, [Int])
readTree (ch:md:r) = let (c, s1) = readNodes ch r
                         (m, s2) = readMeta md s1
                     in
                        (Node c m, s2)

readNodes :: Int -> [Int] -> ([Tree], [Int])
readNodes 0 xs = ([], xs)
readNodes c xs = (nn:a, b)
    where (nn, s1) = readTree xs
          (a, b)   = readNodes (c-1) s1
        
readMeta :: Int -> [Int] -> ([Int], [Int])
readMeta 0 xs     = ([], xs)
readMeta c (x:xs) = (x:ys, r)
    where (ys, r) = readMeta (c-1) xs

foldTree :: ([b] -> b) -> ([Int] -> b) -> (b -> b -> b) -> Tree -> b
foldTree fc fm fz = m
    where m (Node cs ms) = fz (fc $ map m cs) (fm ms)

secondCheck :: Tree -> Int
secondCheck (Node [] ms)       = sum ms
secondCheck (Node cs [])       = 0
secondCheck (Node cs (m:ms)) | m-1 >= l    = secondCheck (Node cs ms)
                             | otherwise = secondCheck (cs!!(m-1)) + secondCheck (Node cs ms)
    where l = length cs

-- First part
ch1 = do
    contents <- readFile "inp8a.txt"
    return $ foldTree sum sum (+) $ fst $ readTree $ parse contents

-- Second part
ch2 = do
    contents <- readFile "inp8a.txt"
    return $ secondCheck $ fst $ readTree $ parse contents
