import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO (Int, Int)
main = do
    input <- readInput
    let s1 = resolve1 input
    let s2 = resolve2 input
    return (s1,s2)

readInput :: IO [Int]
readInput = do
    content <- readFile "app/day10/input2"
    return (map read $ lines content)

resolve1 :: [Int] -> Int
resolve1 l = 
    let sorted = sort l
        joltages = zipWith (-) sorted (0:sorted)
        (a,b,c) = apears joltages (0,0,0) in
        if c /= 0 then
            -1
        else
            a*(b+1)

apears :: [Int] -> (Int, Int, Int) -> (Int, Int, Int)
apears [] acc = acc
apears (1:xs) (a,b,c) = apears xs (a+1,b,c)
apears (3:xs) (a,b,c) = apears xs (a,b+1,c)
apears (_:xs) (a,b,c) = apears xs (a,b,c+1)

resolve2 :: [Int] -> Int
resolve2 l = 
    let sorted = sort l
        posiblePermuts = map (\x -> x-1) $ map length $ splitOn [3] $ zipWith (-) sorted (0:sorted) in
        product $ map combine posiblePermuts

combine :: Int -> Int
-- combine 0 = 1
-- combine 1 = 2
-- combine 2 = 4
-- combine n = combine (n-1) + combine (n-2) + combine (n-3)
combine n
    | n >= 0 = combines !! n
    | otherwise = 1

combines :: [Int]
combines = 1 : 2 : 4: zipWith3 sum3 combines (tail combines) (drop 2 combines)

sum3 :: Num a => a -> a -> a -> a
sum3 a b c = a + b + c
