readInput :: IO [(String, Int, Int)]
readInput = do
    contents <- readFile "app/day08/input"
    let differentLines = lines contents
    return (map (\l -> let [c,n] = words l in (c, read (clean n) :: Int,0)) differentLines)

clean :: String -> String
clean ('+':xs) = xs
clean s = s

run :: [(String, Int, Int)] -> Int -> Int -> Int
run program pc acc
    | pc >= length program = acc
    | i >= 1 = acc
    | comand == "nop" = run updatedProgram (pc+1) acc
    | comand == "jmp" = run updatedProgram (pc+mount) acc
    | comand == "acc" = run updatedProgram (pc+1) (acc+mount)
    where 
        (before, rest) = splitAt pc program
        (comand, mount, i) = head rest
        updatedProgram = before ++ [(comand, mount, i + 1)] ++ tail rest

listIndex program = [x | (x, (comand,_,_)) <- zip [0..] program, comand == "nop" || comand =="jmp"]

run2 :: [(String, Int, Int)] -> Int -> Int -> [Int] -> Int
run2 program pc acc list
    | pc >= length program = acc
    | i >= 1 = run2 (cleanP program) 0 0 (tail list)
    | h == pc && comand == "nop" = run2 updatedProgram (pc+mount) acc list
    | h == pc && comand == "jmp" = run2 updatedProgram (pc+1) acc list
    | comand == "nop" = run2 updatedProgram (pc+1) acc list
    | comand == "jmp" = run2 updatedProgram (pc+mount) acc list
    | comand == "acc" = run2 updatedProgram (pc+1) (acc+mount) list
    where 
        (before, rest) = splitAt pc program
        (comand, mount, i) = head rest
        updatedProgram = before ++ [(comand, mount, i + 1)] ++ tail rest
        h = head list

cleanP :: [(String, Int, Int)] -> [(String, Int, Int)]
cleanP = map (\(c, m, _) -> (c, m, 0))
