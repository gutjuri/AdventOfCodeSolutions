{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import Data.Char
import Data.Maybe
import Data.List (isPrefixOf)

parseLine :: String -> Int
parseLine = go (-1) (-1)
    where 
        go f l [] = f*10+l
        go (-1) _ (x:xs)
            | isDigit x = go (digitToInt x) (digitToInt x) xs
            | otherwise = go (-1) (-1) xs
        go f l (x:xs)
            | isDigit x = go f (digitToInt x) xs
            | otherwise = go f l xs

parseLine2 :: String -> Int
parseLine2 = go (-1) (-1)
    where 
        go f l [] = f*10+l
        go (-1) _ (x:xs)
            | isDigit x = go (digitToInt x) (digitToInt x) xs
            | otherwise = case getFirstWordNum (x:xs) of
                Nothing -> go (-1) (-1) xs
                Just n  -> go n n xs
        go f l (x:xs)
            | isDigit x = go f (digitToInt x) xs
            | otherwise = case getFirstWordNum (x:xs) of
                Nothing -> go f l xs
                Just n  -> go f n xs

getFirstWordNum :: String -> Maybe Int
getFirstWordNum xs = listToMaybe $ catMaybes $ map (\(w, n) -> if w `isPrefixOf` xs then Just n else Nothing) pairs
    where
        pairs = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]

main = do
    inp <- readFile "2023/in01.txt"
    print $ sum $ map parseLine $ lines inp
    print $ sum $ map parseLine2 $ lines  inp