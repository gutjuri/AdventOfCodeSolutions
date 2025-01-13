module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char
import Data.Bifunctor
import Data.List

data Expr = Literal Int | Pair Expr Expr deriving Show

type Parser = Parsec Void String

parseInp :: Parser [Expr]
parseInp = do
    exp <- parseExpr `endBy` (char '\n')
    eof
    return exp

parseLiteral :: Parser Expr
parseLiteral = Literal . digitToInt <$> satisfy (\x -> x >= '0' && x <= '9')

parseExpr :: Parser Expr
parseExpr = do
    _ <- satisfy (=='[')
    leftE <- parseLiteral <|> parseExpr
    _ <- satisfy (==',')
    rightE <- parseLiteral <|> parseExpr
    _ <- satisfy (==']')
    return $ Pair leftE rightE


add :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
add a b = reduce $ map (second (+1)) $ a ++ b 

rule1 :: [(Int, Int)] -> (Bool, [(Int,Int)])
rule1 expr = case findIndices ((>=4) . snd) expr of
    [] -> (False, expr)
    (a:_:_) -> (True, explodeAt a expr)

explodeAt :: Int -> [(Int, Int)] -> [(Int, Int)]
explodeAt 0 ((x, dx):(y, dy):(z, dz):xs) = (0, dx-1) : (y+z, dz) : xs 
explodeAt 1 ((x, dx):(y, dy):(z, dz):(w,dw): xs) = (x+y, dx):(0, dy-1):(z+w, dw):xs
explodeAt 1 ((x, dx):(y, dy):(z, dz):[]) = (x+y, dx):(0, dy-1):[]
explodeAt n (x:xs) = x : explodeAt (n-1) xs

rule2 :: [(Int, Int)] -> (Bool, [(Int, Int)])
rule2 expr = case findIndices ((>= 10) . fst) expr of
    [] -> (False, expr)
    (x:_) -> (True, splitNrAt x expr)

splitNrAt :: Int -> [(Int, Int)] -> [(Int, Int)]
splitNrAt 0 ((x, dx):xs) = (x `div` 2, dx+1) : (x `div` 2 + if even x then 0 else 1, dx+1) :xs
splitNrAt n (x:xs) =x : splitNrAt (n-1) xs


reduce :: [(Int, Int)] -> [(Int, Int)] 
reduce e = case rule1 e of
    (True, e') -> reduce e'
    (False, e') -> case rule2 e' of
        (True, e'') -> reduce e''
        (False, e'') -> e''

parseInput :: String -> [(Int, Int)]
parseInput = go 0
    where 
        go x ('[':xs) = go (x+1) xs
        go x (']':xs) = go (x-1) xs
        go x (',':xs) = go x xs
        go x (n:xs) =  (digitToInt n, x) : go x xs 
        go _ [] = []

p1 :: [[(Int, Int)]] -> [(Int, Int)]
p1 = foldl1' add

main :: IO ()
main = do
    inp <- map parseInput . lines <$> readFile "in18.txt"
    print $ p1 inp -- add (inp!!0) (inp!!1)