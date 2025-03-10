import Data.List.Split (splitOn)
import Data.List (intersect)

readInput :: IO [(String, [(String, Int)])]
readInput = do
    contents <- readFile "app/day07/input"
    diferentsLines = lines contents
    return map transformLine diferentsLines

transformLine :: String -> (String, [(String, Int)])
transformLine line = 
    let [firts, second]= splitOn " bags contain " in
    (firts, transSecond second)

transSecond :: String -> [String, Int]
transSecond s =
    let s' = take 1 (splitOn "." s)
    branchs = split ", " s'

