p1Nextstep :: [String] -> Int -> Int
p1Nextstep [] _ = 0
p1Nextstep (ln : ls) pos
  | ln !! pos == '#' = 1 + p1Nextstep ls ((pos + 3) `mod` length ln)
  | otherwise        = p1Nextstep ls ((pos + 3) `mod` length ln)

p2Nextstep :: [String] -> Int -> Int -> Int -> Int
p2Nextstep [] _ _ _ = 0
p2Nextstep (ln : ls) pos dx dy
  | ln !! pos == '#'
  = 1 + p2Nextstep (drop (dy - 1) ls) ((pos + dx) `mod` length ln) dx dy
  | otherwise
  = p2Nextstep (drop (dy - 1) ls) ((pos + dx) `mod` length ln) dx dy

main :: IO ()
main = do
  ls <- lines <$> getContents
  print $ p1Nextstep ls 0
  print $ product
    [ p2Nextstep ls 0 1 1
    , p2Nextstep ls 0 3 1
    , p2Nextstep ls 0 5 1
    , p2Nextstep ls 0 7 1
    , p2Nextstep ls 0 1 2
    ]
