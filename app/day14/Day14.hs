import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import qualified Data.Map.Strict as Map
main :: IO ()
main = do
    input <- readInput
    let s1 = resolve1 input Map.empty ""
    let s2 = resolve2 input
    print (s1, s2)

readInput :: IO [String]
readInput = do
    content <- readFile "app/day14/input"
    return (map words $ lines content)

resolve1 :: [[String]] -> Map.Map -> String -> Int
resolve1 (["mask", _, m],xs) mem mask = resolve1 xs mem m
resolve1 ([cmd,_, n],xs) mem mask = resolve1 xs newMem mask
    where newMem = mem.insert nn (opMask ) mem
    nn = toBinary36 $ read n

resolve2 :: [String] -> Int
resolve2 _ = 0

opMask :: String -> Int -> Int
opMask l i = toReverse36Int $ reverse $ zipWith opMaskBit l (toBinary36 i)

-- to36binary :: Int -> String
-- to36binary 0 = "0"
-- to36binary 1 = "1"
-- to36binary n
--     | n `rem` 2 == 0 = to36binary

toBinary36 :: Int -> String
toBinary36 n =
    let bin = showIntAtBase 2 intToDigit n ""
        padding = replicate (36 - length bin) '0'
     in padding ++ bin

toReverse36Int :: String -> Int
toReverse36Int "" = 0
toReverse36Int ('0' : xs) = 2 * toReverse36Int xs
toReverse36Int ('1' : xs) = 1 + 2 * toReverse36Int xs
toReverse36Int (_ : _) = error "NOT CORRECT NUMBER"

opMaskBit :: Char -> Char -> Char
opMaskBit 'X' i = i
opMaskBit '1' _ = '1'
opMaskBit '0' _ = '0'
opMaskBit _ _ = error "NOT CORRECT MASK"
