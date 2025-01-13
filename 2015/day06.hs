import Data.Set (Set)
import qualified Data.Set as S


type Point = (Int, Int)

data CommandType = On | Off | Toggle
  deriving (Show)

type Command = (CommandType, Point, Point)

readP :: String -> Point
readP = (\[a, b] -> (a, b)) . map read . words . map (\x -> if x == ',' then ' ' else x)

parseLine :: String -> Command
parseLine = parse' . words
  where
    parse' ["turn", "on", a, _, b] = (On, readP a, readP b)
    parse' ["turn", "off", a, _, b] = (Off, readP a, readP b)
    parse' ["toggle", a, _, b] = (Toggle, readP a, readP b)

execCommands :: [Command] -> Set Point
execCommands = foldl exec S.empty

between :: Point -> Point -> Set Point
between (x, y) (x', y') = S.fromList [(a, b) | a <- [x..x'], b <- [y..y']]

exec :: Set Point -> Command -> Set Point
exec s (On, a, b)     = S.union s (between a b)
exec s (Off, a, b)    = s S.\\ (between a b)
exec s (Toggle, a, b) = s S.\\ toRemove `S.union` toInsert
  where
    (toRemove, toInsert) = S.partition (flip S.member s) (between a b)

main :: IO ()
main = do
  commands <- map parseLine . lines <$> readFile "input/in6.txt"
  print $ S.size $ execCommands commands