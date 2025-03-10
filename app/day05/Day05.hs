import Data.List.Split (splitOn)
readInput :: IO [String]
readInput = do
    contents <- readFile "app/day05/input"
    return (splitOn "\n" contents)

transform :: [String] -> [(Int, Int)]
transform list = map localice list

localice :: String -> (Int, Int)
localice s = (row $ take 7 s, column $ reverse $ take 3 (reverse s))

row :: String -> Int
row "" = 0
row ('F':xs) = row xs
row ('B':xs) = 2 ^ length xs + row xs
row _ = 0


column :: String -> Int
column "" = 0
column ('L':xs) = column xs
column ('R':xs) = 2 ^ length xs + column xs
column _ = 0

resolve1 :: [(Int, Int)] -> Int
resolve1 l = maxList $ map (\(r,c) -> 8*r + c) l

maxList :: Ord a => [a] -> a
maxList [x] = x
maxList (x:x':xs) = maxList ((max x x'):xs)

resolve2 :: [(Int, Int)] -> Int
-- resolve2 l = quicksort $ map (\(r,c) -> 8*r + c) l
resolve2 lt = maxList [x | x<-[1..maxList l], not (x `elem` l), (x+1) `elem` l, (x-1) `elem` l]
  where l = map (\(r,c) -> 8*r + c) lt

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]
