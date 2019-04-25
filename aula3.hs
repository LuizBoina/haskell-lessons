qualBigger x y = if x > y then x else if x < y then y else -1
notas xs = sum xs / fromIntegral(length xs) >= 5.0 
notas' xs = if length xs == 0 then "trola nao chefia" else
			if sum xs / fromIntegral(length xs) >= 5.0 then 
			"ta show" else "nao ta legal"
ordemDeDois a b = if a>b then "ta caindo" else if b>a then "ta descendo" else "e igual"
{-
show da manha
-}

triangulo a b c
    | nTriangulo a b c = nope
    | equilatero a b c = eq
    | escaleno a b c = es 
    | otherwise = isos
	where
		nTriangulo a b c = a==0 || b==0 || c==0
		equilatero a b c = a == b && b == c
		escaleno a b c = a /= b && b /= c && a /= c
		(nope, eq, es, isos) = ("nao eh triangulo", "equilatero", "escaleno", "isosceles")
		
(\\) :: Bool -> Bool -> Bool
True \\ _ = True
_ \\ True = True
_ \\ _ = False

fat 0 = 1
fat n = n * fat (n-1)

pow x 0 = 1
pow x y = x * pow x (y-1)

pow' xs y = [pow x y | x <- xs]

last' [x] = x
last' (_:xs) = last xs

init' [] = []
init' [x] = []
init' (x:xs) = [x] ++ init' xs

cauda1 [x] = []
cauda1 (x:xs) = xs

cauda2 (x:xs) = if length xs == 1 then [] else xs
