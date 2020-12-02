import           Data.List
import           Data.Bits

data Rule = Rule {
    minAppearences :: Int,
    maxAppearances :: Int,
    letter :: Char }

readRule :: String -> Rule
readRule str = Rule (read minStr) (read maxStr) l
 where
  (minStr, _ : r  ) = break (== '-') str
  (maxStr, _ : [l]) = break (== ' ') r


readRulePw :: String -> (Rule, String)
readRulePw str = (readRule ruleStr, pw)
  where (ruleStr, _ : _ : pw) = break (== ':') str

count :: Eq a => a -> [a] -> Int
count x = length . elemIndices x

validate :: Rule -> String -> Bool
validate r pw = occs >= minAppearences r && occs <= maxAppearances r
  where occs = count (letter r) pw

(!?) :: [a] -> Int -> Maybe a
(x : _ ) !? 0 = Just x
(_ : xs) !? n = xs !? (n - 1)
_        !? _ = Nothing

validate2 :: Rule -> String -> Bool
validate2 r pw =
  (pw !? (minAppearences r - 1) == Just (letter r))
    `xor` (pw !? (maxAppearances r - 1) == Just (letter r))

main :: IO ()
main = do
  input <- map readRulePw . lines <$> getContents
  print $ length $ filter (uncurry validate) input
  print $ length $ filter (uncurry validate2) input
