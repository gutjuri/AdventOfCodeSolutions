module Main where

import           Data.SBV
import Data.Char

data Reg = X | Y | Z | W deriving (Show)

data InstrType = Inp | Add | Mul | Div | Mod | Eql deriving (Show)

data Instr = Instr InstrType Reg (Either Reg Int) deriving (Show)

parseInp ::  String -> [Instr]
parseInp = map parseInstr . lines
    where
        parseInstr ln = case take 3 ln of
            "inp" -> Instr Inp (readReg (ln!!4)) $ Right 0
            "add" -> Instr Add (readReg (ln!!4)) $ readRegOrInt (drop 6 ln)
            "mul" -> Instr Mul (readReg (ln!!4)) $ readRegOrInt (drop 6 ln)
            "div" -> Instr Div (readReg (ln!!4)) $ readRegOrInt (drop 6 ln)
            "mod" -> Instr Mod (readReg (ln!!4)) $ readRegOrInt (drop 6 ln)
            "eql" -> Instr Eql (readReg (ln!!4)) $ readRegOrInt (drop 6 ln)
        readReg c = case c of
            'x' -> X
            'y' -> Y
            'z' -> Z
            'w' -> W
        readRegOrInt s | isAlpha $  head s = Left $ readReg $ head s
                       | otherwise = Right $ read s
 
doSCalc :: [Instr] -> Bool
doSCalc = go (0,0,0,0)
    go (_, _, z, _) [] = z == 0
    go state (i:is) = go (doInstr state i)


main :: IO ()
main = undefined
