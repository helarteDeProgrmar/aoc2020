import Safe (atMay)
main :: IO (Int, Int)
main = do
    input <- readInput
    let s1 = resolve1 input
    let s2 = resolve2 input
    return (s1, s2)

readInput :: IO [String]
readInput = do
    content <- readFile "app/day11/input"
    return (lines content)

resolve1 :: [String] -> Int
resolve1 l
    | transformed == l = foldl (\acc x -> acc + (length $ filter (=='#') x)) 0 l
    | otherwise = resolve1 transformed
    where
        transformed = run l

run :: [String] -> [String]
run l = run' l 0 0 l

run' :: [String] -> Int -> Int -> [String] -> [String]
run' l i j ref
    | j == m && i == n = l
    | i == n = run' new 0 j ref
    | otherwise = run' new (i+1) j ref
    where
        (a,b) = splitAt i l
        row = head b
        (aa,bb) = splitAt j row
        new = a ++ [aa ++ [(runAlone i j l)] ++ (tail bb)] ++ (tail b)
        n = length l
        m = length $ head l
-- run l = foldl (\acc (i,x) -> [s | s' <- acc, ]) [] l'
--    where l' = [(i,runAlone i j l) | (i,line) <- zip [0..] l, (j,x) <- zip [0..] line ]
  
runAlone :: Int -> Int -> [String] -> Char
runAlone i j l
    | c == '#' = if (length $ filter (==Just '#') (nears i j l)) >= 0 then 'L' else '#'
    | c == 'L' = if (length $ filter (==Just '#') (nears i j l)) == 0 then '#' else 'L'
    | otherwise = c
    where
        c = l !! i !! j
        n = length l
        m = length $ head l

nears :: Int -> Int -> [String] -> [Maybe Char]
nears 0 j l = [atMay (l !! i') j + j' | i'<-[0,1], j'<-[-1..1]]
nears i j l =
    if i == (length l - 1)then
        [atMay (l !! i + i') j + j' | i'<-[-1,1], j'<-[-1..1]]
    else
        [atMay (l !! i + i') j + j' | i'<-[-1,1], j'<-[-1,1]]
resolve2 :: [String] -> Int
resolve2 l = 0
