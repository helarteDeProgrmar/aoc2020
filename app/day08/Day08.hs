import Data.List.Split (splitOn)

readInput :: IO [(String, Int, Int)]
readInput = do
    contents <- readFile "app/day08/input"
    let differentLines = lines contents
    return (map (\l -> let [c,n] = words l in (c, read (clean n) :: Int,0)) differentLines)

clean :: String -> String
clean "" = ""
clean ('+':xs) = xs
clean s = s

run :: [(String, Int, Int)] -> Int -> Int -> Int
run program pc acc
    | i >= 1 = acc
    | comand == "nop" = run updatedProgram (pc+1) acc
    | comand == "jmp" = run updatedProgram (pc+mount) acc
    | comand == "acc" = run updatedProgram (pc+1) (acc+mount)
    where 
        (before, rest) = splitAt pc program
        (comand, mount, i) = head rest
        updatedProgram = before ++ [(comand, mount, i + 1)] ++ tail rest
run _ _ _ = -666
