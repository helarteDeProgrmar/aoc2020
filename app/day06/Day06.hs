import Data.List.Split (splitOn)
import Data.List (intersect)
readInput :: IO [[String]]
readInput = do
    contents <- readFile "app/day06/input"
    return (map (\s -> splitOn "\n" s) $ splitOn "\n\n" contents)

resolve1 :: [[String]] -> Int
resolve1 list = 
    let flatenLists = map (\l -> foldl (++) "" l) list
        filterList = map dropSame flatenLists in
        foldl (+) 0 ( map length filterList)

dropSame :: Eq a => [a] -> [a]
dropSame [] = []
dropSame (x:xs)
    | x `elem`  xs = dropSame xs
    | otherwise = x : dropSame xs

resolve2 :: [[String]] -> Int
resolve2 list =
    let flatenLists = map (\l -> foldl (++) "" l) list
        intersectionList = 
            map (\(total, l) -> foldl intersect (dropSame total) l) (zip (dropSame flatenLists) list) in
        foldl (+) 0 (map length intersectionList)
