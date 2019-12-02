import qualified Data.Set as S
import           Data.List

banned :: [String]
banned = ["ab", "cd", "pq", "xy"]

vowels :: String
vowels = "aeiou"

nice :: String -> Bool
nice = nice' 0 False
  where
    nice' :: Int -> Bool -> String -> Bool
    nice' vc d []       = vc >= 3 && d
    nice' vc d (x:[])   = if x `elem` vowels then nice' (vc+1) d [] else nice' (vc) d []
    nice' vc d (a:b:xs) | [a, b] `elem` banned = False
                        | a `elem` vowels      = nice' (vc+1) (d||a==b) (b:xs)
                        | otherwise            = nice' vc (d||a==b) (b:xs)

nice2 :: String -> Bool
nice2 s = doublePair s && triplet s

triplet (a:b:c:xs) = (a == c) || triplet (b:c:xs)
triplet _ = False
    
doublePair (x:y:xs) = ((>0) . length . filter (==[x,y]) . map (take 2) . tails) xs || doublePair (y:xs)
doublePair _ = False

main :: IO ()
main = do
  strs <- lines <$> readFile "input/in5.txt"
  let ct = length $ filter nice strs
  putStrLn $ show ct
  let ct2 = length $ filter nice2 strs
  putStrLn $ show ct2
  putStrLn $ show $ nice2 "ieodomkazucvgmuy"