import System.IO (readFile)

main :: IO ()
main = do
    -- Lee el contenido del archivo "ejemplo.txt" y lo guarda en contents
    contents <- readFile "ejemplo.txt"
    
    -- Imprime el contenido en pantalla
    putStrLn contents
