import Data.List.Split (splitOn)
main :: IO ()
main = do
    input <- readInput
    let s1 = resolve1 input
    let s2 = resolve2 input
    print (s1, s2)

readInput :: IO (Int,[Int])
readInput = do
    content <- readFile "app/day13/input"
    let [x,xs] = lines content
    return ((read x, map read $ splitOn "," xs))

resolve1 :: (Int,[Int]) -> Int
resolve1 _ = 0

resolve2 :: (Int,[Int]) -> Int
resolve2 _ = 0
