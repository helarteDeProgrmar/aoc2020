main :: IO ()
main = do
    input <- readInput
    let s1 = resolve1 input (0, 0) 0
    let s2 = resolve2 input (0,0) (10,1)
    print (s1, s2)

readInput :: IO [(Char, Int)]
readInput = do
    contents <- readFile "app/day12/input"
    return (map (\(x : xs) -> (x, read xs)) $ lines contents)

resolve1 :: [(Char, Int)] -> (Int, Int) -> Int -> Int
resolve1 [] (x, y) _ = abs (x) + abs (y)
resolve1 ((i, mount) : xs) (x, y) o
    | i == 'N' = resolve1 xs (x, y + mount) o
    | i == 'S' = resolve1 xs (x, y - mount) o
    | i == 'E' = resolve1 xs (x + mount, y) o
    | i == 'W' = resolve1 xs (x - mount, y) o
    | i == 'L' = resolve1 xs (x, y) (rem (o + mount) 360)
    | i == 'R' = resolve1 xs (x, y) (rem (o + 360 - mount) 360)
    | i == 'F' =
        case o of
            0 -> resolve1 xs (x + mount, y) o
            90 -> resolve1 xs (x, y + mount) o
            180 -> resolve1 xs (x - mount, y) o
            270 -> resolve1 xs (x, y - mount) o
            _ -> error "rare rotating"
    | otherwise = error (i : "instruction not macht")

resolve2 :: [(Char, Int)] -> (Int, Int) -> (Int,Int) -> Int
resolve2 [] (x, y) _ = abs (x) + abs (y)
resolve2 ((i, mount) : xs) (x, y) (u,v)
    | i == 'N' = resolve2 xs (x, y) (u, v + mount)
    | i == 'S' = resolve2 xs (x, y) (u, v - mount)
    | i == 'E' = resolve2 xs (x , y) (u + mount, v)
    | i == 'W' = resolve2 xs (x , y) (u - mount, v)
    | i == 'R' = 
        case mount of
            0 -> resolve2 xs (x, y) (u, v)
            90 -> resolve2 xs (x, y) (v, -u)
            180 -> resolve2 xs (x, y) (-u, -v)
            270 -> resolve2 xs (x, y) (-v, u)
            _ -> error "rare rotating"
    | i == 'L' =
        case mount of
            0 -> resolve2 xs (x, y) (u, v)
            90 -> resolve2 xs (x, y) (-v, u)
            180 -> resolve2 xs (x, y) (-u, -v)
            270 -> resolve2 xs (x, y) (v, -u)
            _ -> error "rare rotating"
    | i == 'F' = resolve2 xs (x+(mount*u), y+(mount*v)) (u,v)
    | otherwise = error (i : "instruction not macht")
