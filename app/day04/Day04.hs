import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Char (isDigit, isHexDigit)

readInput :: IO [[String]]
readInput = do
    contents <- readFile "app/day04/input"
    return (map words (splitOn "\n\n" contents))

resolve1 :: [[String]] -> Int
resolve1 list = length [ x | x <- list, correct_passport x]

resolve2 :: [[(String, String)]] -> Int
resolve2 list = length [x | x <- list, correctPassport x]

correct_passport :: [String] -> Bool
correct_passport list = (length list) == 8 
    || ((length list) == 7 &&  not (elem "cid" ( map (\x -> head (splitOn ":" x)) list )))

transformData :: [[String]] -> [[(String,String)]]
transformData = map ( map (\x -> let (k:v:_) = splitOn ":" x in (k,v)) )

correctPassport :: [(String, String)] -> Bool
correctPassport list = 
    let requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        keyPresent = map fst list
    in (length list == 8 || length list == 7)
        && all (\x -> x `elem` keyPresent) requiredKeys
        && all validate list

validate :: (String, String) -> Bool
validate ("byr", val) = validateYear val 1920 2002
validate ("iyr", val) = validateYear val 2010 2020
validate ("eyr", val) = validateYear val 2020 2030
validate ("hgt", val) = validateHeight val
validate ("hcl", val) = validateHairColor val
validate ("ecl", val) = val `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
validate ("pid", val) = length val == 9 && all isDigit val
validate ("cid", _)   = True
validate _            = False

validateYear :: String -> Int -> Int -> Bool
validateYear s a b = case readMaybe s of
    Just y  -> length s == 4 && y >= a && y <= b
    Nothing -> False

validateHeight :: String -> Bool
validateHeight s
    | "cm" `suffixOf` s = let h = readMaybe (take (length s - 2) s) in
                            case h of
                                Just y -> y >= 150 && y <= 193
                                Nothing -> False
    | "in" `suffixOf` s = let h = readMaybe (take (length s - 2) s) in
                            case h of
                                Just y -> y >= 59 && y <= 76
                                Nothing -> False
    | otherwise = False
  where
    suffixOf suf str = suf == drop (length str - length suf) str

validateHairColor :: String -> Bool
validateHairColor ('#':xs) = length xs == 6 && all (`elem` "0123456789abcdef") xs
validateHairColor _        = False
