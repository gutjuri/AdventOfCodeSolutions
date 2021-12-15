{-# LANGUAGE FlexibleContexts #-}

module Day18 where

import           Text.Parsec.Expr
import           Text.Parsec.String
import           Text.Parsec.Char
import qualified Text.Parsec.Token             as L
import           Text.Parsec
import           Data.Either
import           Data.Char

data Expr = Add Expr Expr | Mul Expr Expr | Val Integer
  deriving Show

integer :: Parser Integer
integer = do
  i <- many digit
  --spaces
  return $ read i

parens :: Parser Expr -> Parser Expr
parens = between (char '(') (char ')')

term = parens expr <|> Val <$> integer

term2 = parens expr2 <|> Val <$> integer

table = [[binary "*" Mul AssocLeft, binary "+" Add AssocLeft]]

table2 = [[binary "+" Add AssocLeft], [binary "*" Mul AssocLeft]]

--binary :: String -> (Expr -> Expr -> Expr) -> Assoc -> _
binary name fun = Infix
  (do
    string name
    return fun
  )

expr :: Parser Expr
expr = buildExpressionParser table term

expr2 :: Parser Expr
expr2 = buildExpressionParser table2 term2

evalExpr :: Expr -> Integer
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
evalExpr (Val v  ) = v

evalLine :: String -> Integer
evalLine = evalExpr . fromRight (Val 0) . parse expr ""

evalLine2 :: String -> Integer
evalLine2 = evalExpr . fromRight (Val 0) . parse expr2 ""

day18 :: IO ()
day18 = do
  putStrLn "Day18"
  inp <- map (concat . words) . lines <$> readFile "in18.txt"
  print $ sum $ map evalLine inp
  print $ sum $ map evalLine2 inp
