module Main where

import Control.Monad (replicateM)
import Data.Char (digitToInt)
import Data.List
import Data.Void
import Text.Megaparsec

type Version = Int

type TypeId = Int

data Packet = Literal Version Int | Operator Version TypeId [Packet] deriving (Show)

type Parser = Parsec Void String

parseAllPackets :: Parser [Packet]
parseAllPackets = do
  many
    ( do
        p <- parsePacket
        try (many (single '0') >> eof)
        return p
    )

parsePacket :: Parser Packet
parsePacket = do
  rest <- getInput
  versionNr <- fromBin <$> takeP Nothing 3
  typeId <- fromBin <$> takeP Nothing 3
  case typeId of
    4 -> parseLiteralPacket versionNr
    n -> parseOperator versionNr n

parseLiteralPacket :: Version -> Parser Packet
parseLiteralPacket v = do
  Literal v . fromBin <$> parseContents
  where
    parseContents :: Parser String
    parseContents = do
      (x : xs) <- takeP Nothing 5
      if x == '0'
        then do
          return xs
        else do
          rest <- parseContents
          return $ xs ++ rest

parseUntilSrcPos :: Int -> Parser [Packet]
parseUntilSrcPos n = go
  where
    go = do
      pos <- getOffset
      if pos == n
        then return []
        else do
          packet <- parsePacket
          rest <- go
          return $ packet : rest

parseOperator :: Version -> TypeId -> Parser Packet
parseOperator v tid = do
  lengthTypeId <- takeP Nothing 1
  if lengthTypeId == "0"
    then do
      lengthInBits <- fromBin <$> takeP Nothing 15
      pos <- getOffset
      packets <- parseUntilSrcPos (pos + lengthInBits)
      return $ Operator v tid packets
    else do
      nrOfSubpackets <- fromBin <$> takeP Nothing 11
      subpackets <- replicateM nrOfSubpackets parsePacket
      return $ Operator v tid subpackets

toBin :: Int -> String
toBin 0 = "0"
toBin n = toBin (n `quot` 2) ++ show (n `rem` 2)

fromBin :: String -> Int
fromBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

makeLen :: Int -> String -> String
makeLen n s
  | length s == n = s
  | length s > n = drop (length s - n) s
  | otherwise = replicate (n - length s) '0' ++ s

hexToBinString :: String -> String
hexToBinString = concatMap (makeLen 4 . toBin . digitToInt)

versionNrSum :: [Packet] -> Int
versionNrSum = sum . map f
  where
    f (Literal v _) = v
    f (Operator v _ ps) = v + versionNrSum ps

eval :: Packet -> Int
eval (Literal _ i) = i
eval (Operator _ 0 ps) = sum $ map eval ps
eval (Operator _ 1 ps) = product $ map eval ps
eval (Operator _ 2 ps) = minimum $ map eval ps
eval (Operator _ 3 ps) = maximum $ map eval ps
eval (Operator _ 5 ps) = if eval (ps !! 0) > eval (ps !! 1) then 1 else 0
eval (Operator _ 6 ps) = if eval (ps !! 0) < eval (ps !! 1) then 1 else 0
eval (Operator _ 7 ps) = if eval (ps !! 0) == eval (ps !! 1) then 1 else 0

main :: IO ()
main = do
  inp <- delete '\n' <$> readFile "in16.txt"
  case parse parseAllPackets "" $ hexToBinString inp of
    Left e -> print e
    Right packets -> do
      print $ versionNrSum packets
      print $ eval $ head packets
