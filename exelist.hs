lcm' 0 _ = 0
lcm' _ 0 = 0
lcm' x y =  abs ((x `quot` (gcd x y)) * y)

repeated [] = False
repeated (x:xs) = (or [x==y | y<-xs]) || repeated xs

concat' xs ys = noRepeat ([z | z<-(xs++ys)]) []
               where noRepeat [] ws = ws
                     noRepeat (z:zs) ws
                              | repeated (ws++[z]) = noRepeat zs ws
                              | otherwise = noRepeat zs (ws++[z]) 

four xs = (maior xs, menor xs, pares xs , impares xs)
pares xs = [x | x<-xs, even x]
impares xs = [x | x<-xs, (not.even) x]
maior [x] = x
maior (x:xs)
      | x > (maior xs) = x
      | otherwise = maior xs
menor [x] = x
menor (x:xs)
      | x < (menor xs) = x
      | otherwise = menor xs

neperNat x = 2.71828**x

fac 0 = 1
fac n = n * fac (n-1)
neperZeroUm x n = if (x**n/(fac n) - x**(n+1)/(fac (n+1)) < 0.00001) then x**n/(fac n) else (x**n/(fac n) + (neperZeroUm x (n+1)))
neperZeroUm' x = limitNeper [x**n/(fac n) | n<-[0..100]]
                 where limitNeper (x:xs)
                                | x - (head xs) < 0.00001 = x
                                | otherwise = x + limitNeper xs
                          
neperReal x = (neperZeroUm' (resto x))*(neperNat (fromIntegral $ truncate x))
              where resto x = x - (fromIntegral $ truncate x )

neperAll x = if x < 0 then 1/(neperReal (-x)) else neperReal x

nMultiplos n a b = take 16 [x | x<-[0..] , x `mod` a == 0, x `mod` b == 0]

potencia1 x n = if n == 0 then 1 else if n > 0 then x*(potencia1 x (n-1)) else (1/x)*(potencia1 x (n+1))
potencia2 x n
        | n == 0 = 1
        | n > 0 = x*(potencia1 x (n-1))
        | otherwise = (1/x)*(potencia1 x (n+1))
  
multiplos m n = [x*m | x<-[0..n], (x*m)<=n]
multiplos' m n = takeWhile (<=n) [x*m | x<-[0..]]
 