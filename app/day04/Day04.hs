import Data.List.Split (splitOn)

readInput :: IO [[String]]
readInput = do
    contents <- readFile "app/day04/input"
    return (map words (splitOn "\n\n" contents))

resolve1 :: [[String]] -> Int
resolve1 list = length [ x | x <- list, correct_passport x]

correct_passport :: [String] -> Bool
correct_passport list = (length list) == 8 
    || ((length list) == 7 &&  not (elem "cid" ( map (\x -> head (splitOn ":" x)) list )))
