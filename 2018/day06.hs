-- Day 6
-- Input pls in inp2a.txt

import Data.Maybe
import Data.List
import qualified Data.Set as Set

type Point = (Int, Int)

parsePoint :: String -> Point
parsePoint str = (read $ takeWhile (/= ',') str , read $ dropWhile (/= ' ') str)

dist :: Point -> Point -> Int
dist (x, y) (x', y') = abs (x-x') + abs (y-y')

closest :: [Point] -> Point -> Maybe Point
closest ps p = closest' p ps []
    where
        closest' _  []     x       = if length x == 1 then Just (head x) else Nothing
        closest' x  (p:ps) []      = closest' x ps [p]
        closest' cp (p:ps) l@(b:_) | dist cp p < dist cp b  = closest' cp ps [p]
                                   | dist cp p == dist cp b = closest' cp ps (p:l)
                                   | otherwise              = closest' cp ps l

isOnEdge :: Int -> Int -> Point -> Bool
isOnEdge maxx maxy (x, y) = x == 0 || y == 0 || x == maxx || y == maxy

infRegions :: Int -> Int -> [(Point, Point)] -> Set.Set Point
infRegions maxx maxy = foldl ir Set.empty
    where
        ir s (q, p) | isOnEdge maxx maxy q = Set.insert p s
                    | otherwise            = s

-- Solution 1  
ch1 = do
    contents <- readFile "inp6a.txt"
    let points = map parsePoint $ lines contents
    let maxx = maximum $ map fst points
    let maxy = maximum $ map snd points
    let allPoints = [(x, y) | x <- [0..maxx], y <- [0..maxy]]
    let regions = map (\(a, Just b) -> (a, b)) $ filter (\(_, x) -> isJust x) $ zip allPoints $ map (closest points) allPoints
    let iR = infRegions maxx maxy regions
    let regionedPoints = filter (\p -> not $ Set.member p iR) $ map snd regions
    return $ maximum $ map length $ groupBy (==) $ sort $ regionedPoints
    
-- Part 2

isInRegion :: [Point] -> Int -> Point -> Bool
isInRegion ps max x = max > (sum $ map (dist x) ps)

-- Solution 2
ch2 = do
    contents <- readFile "inp6a.txt"
    let points = map parsePoint $ lines contents
    let maxx = maximum $ map fst points
    let maxy = maximum $ map snd points
    let allPoints = [(x, y) | x <- [0..maxx], y <- [0..maxy]]
    return $ length $ filter (isInRegion points 10000) allPoints
