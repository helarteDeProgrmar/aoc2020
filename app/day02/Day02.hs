import System.IO
import Data.List.Split (splitOn)

readInput :: IO [(Int, Int, Char, [Char])]
readInput = do
    contents <- readFile "app/day02/input"
    -- let result = map read (lines contents)
    let result = map parseLine (lines contents)
    return [x | Just x <- result]

parseLine :: String -> Maybe (Int, Int, Char, String)
parseLine linea =
  case words linea of
    [range, letter, string] ->
        case splitOn "-" range of
          [start, end] -> Just (read start, read end, head letter, string)
          _ -> Nothing
    _ -> Nothing
resolve1 :: [(Int, Int, Char, String)] -> Int
resolve1 list = length [ () | (start, end, char, string) <- list, 
                             let count = length $ filter (== char) string,
                             start <= count, count <= end ]
                             

resolve2 :: [(Int, Int, Char, String)] -> Int
resolve2 list = length [ () | (start, end, char, string) <- list, 
                         let pos1 = string !! (start - 1)
                             pos2 = string !! (end - 1),
                        (pos1 == char) /= (pos2 == char)]
