-- Day 2
-- Assumes that the input file is saved as "inp2a.txt".

letters = "abcdefghijklmnopqrstuvwxyz"

containsTimes l = foldl (\acc x -> if x == l then acc+1 else acc) 0

occsInString str = foldl toTuple (False, False) $ map (flip containsTimes str) letters
    where
        toTuple (a, b) n | n == 2    = (True, b)
                         | n == 3    = (a, True)
                         | otherwise = (a, b)

                         
                         
checksum xs = f $ foldl (\(a, b) (x, y) -> (a + fromEnum x, b + fromEnum y)) (0, 0) $ map occsInString xs
    where 
        f (a, b) = a * b

-- Solution 1
ch1 = do
    contents <- readFile "inp2a.txt"
    return $ checksum $ lines contents
