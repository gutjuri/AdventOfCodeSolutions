{-# LANGUAGE LambdaCase #-}

module Day04 where

import           Data.List
import           Data.Char

parseInput :: String -> [[(String, String)]]
parseInput =
  map go
    . lines
    . concatMap
        (\case
          "" -> "\n"
          x  -> (' ' : x)
        )
    . lines
  where go = map (break (== ':')) . words

expectedFields :: [String]
expectedFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid :: [(String, String)] -> Bool
isValid fs = sort (fieldsPresent `intersect` expectedFields)
  == sort expectedFields
  where fieldsPresent = map fst fs

isValid2 :: [(String, String)] -> Bool
isValid2 fs = isValid fs && all validateField fs
 where
  validateField ("byr", ':' : val) =
    let val' = read val in val' >= 1920 && val' <= 2002
  validateField ("iyr", ':' : val) =
    let val' = read val in val' >= 2010 && val' <= 2020
  validateField ("eyr", ':' : val) =
    let val' = read val in val' >= 2020 && val' <= 2030
  validateField ("hgt", ':' : val) = valH val
  validateField ("hcl", ':' : val) = valHcl val
  validateField ("ecl", ':' : val) =
    val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  validateField ("pid", ':' : val) =
    length val == 9 && all (`elem` "0123456789") val
  validateField _ = True
  valH hgt =
    let (n, u) = span isDigit hgt
    in  case u of
          "cm" -> read n >= 150 && read n <= 193
          "in" -> read n >= 59 && read n <= 76
          _    -> False
  valHcl ('#' : xs) = length xs == 6 && all (`elem` "0123456789abcdef") xs
  valHcl _          = False

day04 :: IO ()
day04 = do
  putStrLn "Day04"
  inp <- parseInput <$> readFile "in04.txt"
  print $ length $ filter isValid inp
  print $ length $ filter isValid2 inp
