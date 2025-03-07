readInput :: IO [String]
readInput = do
    contents <- readFile "app/day03/input"
    return (lines contents)

resolve1 :: [String] -> Int -> Int-> Int
resolve1 [] _ _ = 0
resolve1 (x:xs) c l
    | (x !! c) == '#' = 1 + resolve1 xs (mod (c + 3) l) l
    | (x !! c) /= '#' = resolve1 xs (mod (c + 3) l) l


resolve2 :: [String] -> Int -> Int -> Int-> Int
resolve2 [] _ _ _ = 0
resolve2 (x:xs) c l r
    | (x !! c) == '#' = 1 + resolve2 xs (mod (c + r) l) l r
    | (x !! c) /= '#' = resolve2 xs (mod (c + r) l) l r

end2 :: [String] -> Int
end2 list = resolve2 list 0 (length (head list)) 1
    *resolve2 list 0 (length (head list)) 3
    *resolve2 list 0 (length (head list)) 5
    *resolve2 list 0 (length (head list)) 7
    *resolve2 [x | (i, x) <- zip [0..] list, even i] 0 (length (head list)) 1
