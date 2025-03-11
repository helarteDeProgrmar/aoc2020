import Data.List.Split (splitOn)

readInput :: IO []
readInput = do
    contents <- readFile "app/day08/input"
    let diferentsLines = lines contents
    return (map transformLine diferentsLines)
