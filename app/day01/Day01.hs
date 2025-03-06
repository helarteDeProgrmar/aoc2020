import System.IO
import Data.List

main :: IO Int
main = giveResult1 2020 =<< readNumbers

main2 :: IO Int
main2 = giveResult2 =<< readNumbers

readNumbers :: IO [Int]
readNumbers = do
    contents <- readFile "app/day01/input1"
    let result = map read (lines contents)
    return result

giveResult1 :: Int -> [Int] -> IO Int
giveResult1 _ [] = return 0
giveResult1 criteria (x:xs) = do
    let complementary = find (\y -> x + y == criteria) xs
    case complementary of
        Just c  -> return (c * x)
        Nothing -> giveResult1 criteria xs

giveResult2 :: [Int] -> IO Int
giveResult2 [] = return 0
giveResult2 [_] = return 0
giveResult2 (x1:x2:xs) = do
    result <- giveResult1 (2020 - x1) (x2:xs)
    case result of
        0 -> giveResult2 (x2:xs)
        a -> return (a * x1)
