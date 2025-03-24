import Data.List.Split (splitOn)
main :: IO ()
main = do
    input <- readInput
    let s1 = resolve1 input
    let (_, input2) = input
    let s2 = resolve2 input2
    print (s1, s2)

readInput :: IO (Int,[Int])
readInput = do
    content <- readFile "app/day13/input"
    let [x,xs] = lines content
    return ((read x, map read $ filter (\x -> x /= "x") $ splitOn "," xs))

resolve1 :: (Int,[Int]) -> Int
resolve1 (timestamp, buses) = 
    head [(x*(y-timestamp)) | (x,y) <- zip buses times, y == minimum times]
    where
        times = map (\x -> (head [n | n<-[timestamp..], rem n x == 0])) buses

resolve2 :: [Int] -> Int
resolve2 _ = 0
