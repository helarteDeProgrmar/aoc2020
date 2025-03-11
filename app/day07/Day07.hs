import Data.List.Split (splitOn)

readInput :: IO [(String, [(String, Int)])]
readInput = do
    contents <- readFile "app/day07/input"
    let diferentsLines = lines contents
    return (map transformLine diferentsLines)

transformLine :: String -> (String, [(String, Int)])
transformLine line = 
    let [firts, second] = splitOn " bags contain " line in
    (firts, transSecond second)

transSecond :: String -> [(String, Int)]
transSecond s =
    let s' = head (splitOn "." s)
        control = head (words s')
        branchs = splitOn ", " s' in
    if length branchs == 1 && control == "no" then
        []
    else
        transSecond' branchs

transSecond' :: [String] -> [(String, Int)]
transSecond' [] = []
transSecond' (x:xs) = 
    let ws = words x in
    (ws !! 1 ++ " " ++ ws !! 2, read $ ws !! 0) : transSecond' xs

resolve1 :: [(String, [(String, Int)])] -> Int
resolve1 list =
    let dependencies = [(x,y) | (x, y') <- list, y <- [n | (n,_) <- y']] in
    countColors ["shiny gold"] dependencies []

countColors :: [String] -> [(String, String)] -> [String] -> Int
countColors [] _ list = length list
countColors (x:xs) depen list =
    let addToList = [n | (n,x') <- depen, x'==x, not (n `elem` list)]
        addKernel = [n | n <- addToList, not(n `elem` xs)] in
    countColors (addKernel ++ xs) depen (list ++ addToList)

resolve2 :: [(String, [(String, Int)])] -> Int
resolve2 list = countBags list ("shiny gold", 1) - 1

countBags :: [(String, [(String, Int)])] -> (String, Int) -> Int
countBags depen (s, n) =
    let (children:_) = [ count | (name, count) <- depen, name==s] in
    n + n*(foldl (+) 0 (map (countBags depen) children))
