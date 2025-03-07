import Data.List.Split (splitOn)

readInput :: IO [String]
readInput = do
    contents <- readFile "app/day04/input"
    return splitOn "\n\n" contents


