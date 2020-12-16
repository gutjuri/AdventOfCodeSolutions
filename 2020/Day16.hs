module Day16 where

import Data.Char
import Data.Bifunctor
import Data.Maybe
import Data.List

data Field = Field 
    { name :: String 
    , range1 :: (Int, Int)
    , range2 :: (Int, Int)
    } deriving (Show, Eq)

type Ticket = [Int]

parseField :: String -> Field
parseField str = Field nm (min1, max1) (min2, max2)
  where 
    (nm, _:rest) = break (==':') str
    [min1, max1, min2, max2] = map read $ words $ map (\c -> if isDigit c then c else ' ') rest

parseTicket :: String -> [Int]
parseTicket = map read . words . map (\c -> if c == ',' then ' ' else c)

parseInp :: String -> ([Field], Ticket, [Ticket])
parseInp str = (fd, ownTicket, nearTickets)
  where
    (fd, _:_:r) = first (map parseField) $ break (=="") $ lines str
    (ownTicket, _:_:r') = first (parseTicket . head) $ break (=="") r
    nearTickets = map parseTicket r'

p1 :: [Field] -> [Ticket] -> Int
p1 fs = sum . concatMap (findInvalidValues fs)

findInvalidValues :: [Field] -> Ticket -> [Int]
findInvalidValues fs = filter isInvalid
  where
    isInvalid x = not (any (fitsIn x) fs)

fitsIn :: Int -> Field -> Bool
fitsIn x (Field _ (min1, max1) (min2, max2)) = x >= min1 && x <= max1
                                               || x >= min2 && x <= max2

fieldPossibilities :: Field -> [Ticket] -> [Int]
fieldPossibilities field ts = foldl' (\ps (i, v) -> if fitsIn v field then ps else delete i ps)
                              [0.. length (head ts) - 1]
                              $ concatMap (zip [0..]) ts

sortOut :: [(Field, [Int])] -> [(Field, Int)]
sortOut [] = []
sortOut assocs = (f, v) : sortOut (map (second (delete v)) $ delete u assocs)
  where
    u@(f, [v]) = fromJust $ find (\(_, x) -> length x == 1) assocs

fieldAssociations :: [Ticket] -> [Field] -> [(Field, Int)]
fieldAssociations ts = sortOut . map (\f -> (f, fieldPossibilities f ts))

p2 :: Ticket -> [(Field, Int)] ->  Int
p2 t fa = product $ map ((t!!) . snd) $ filter (isPrefixOf "departure" . name . fst) fa

day16 :: IO ()
day16 = do
  putStrLn "Day16"
  (fd, ownTicket, nearTickets) <- parseInp <$> readFile "in16.txt"
  print $ p1 fd nearTickets
  let validTickets = filter (null . findInvalidValues fd) (ownTicket:nearTickets)
  print $ p2 ownTicket $ fieldAssociations validTickets fd