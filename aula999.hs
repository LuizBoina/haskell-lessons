import System.IO

main = do putStrLn "Qual a opcao (0-Sair, 1-Somar, 2-Subtrair, 3-Multiplicar)"
          opcao <- getLine
          menu (read opcao :: Int)

menu n
		| n == 0 = putStrLn "SAIR"
		| n == 1 = doCount (+)
		| n == 2 = doCount (-)
		| n == 3 = doCount (*)
    | otherwise = putStrLn "INVALIDO"

doCount f = do putStrLn "digite um numero"
               x <- getLine
               putStrLn "digite outro número"
               y <- getLine
               putStrLn (show (f (read x :: Int) (read y :: Int)))