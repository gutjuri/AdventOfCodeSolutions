module Main where

condition [c]      a b = a && b
condition (c:c':r) a b = condition (c':r) na nb
  where 
    na = a || c == c'
    nb = b && c <= c'

numPossibilities = [x | x <- [235741..706948], condition (show x) False True]

condition2 n = hasIsoDigs ('_':n++"_") && isInc n
  where
    isInc n = fst $ foldl (\(s, lc) c -> (s && lc <= c, c)) (True, '0') n 
    hasIsoDigs (a:b:c:d:r)  = a /= b && b == c && c /= d || hasIsoDigs (b:c:d:r)
    hasIsoDigs _ = False
    
numPossibilities2 = [x | x <- [235741..706948], condition2 (show x)]

main = do 
  print $ length numPossibilities 
  print $ length numPossibilities2
  