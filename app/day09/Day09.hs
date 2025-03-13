readInput :: IO ([Int],[Int])
readInput = do 
    content <- readFile "app/day09/input"
    return (splitAt 25 $ map read $ lines content)

resolve1 :: ([Int],[Int]) -> Int
resolve1 (_, []) = -1
resolve1 (adders, list)
    | length selected == 0 = h
    | otherwise = resolve1 (newAdders, t)
    where
        (h:t) = list
        newAdders = tail adders ++ [h]
        selected = [(x,y) | x <- adders, y <-adders, x+y==h]

resolve2 :: Int -> [Int] -> Int -> Int
resolve2 nOe list index
    | nOe >= length list = -1
    | sum adders == 25918798 = (minimum adders) + (maximum adders)
    | rest == [] = resolve2 (nOe+1) list 0
    | otherwise = resolve2 nOe list (index+1)
    where 
        iter = drop index list
        (adders, rest) = splitAt nOe iter
-- readFile -> lines -> map read
