import Data.Bits
prod = \x->(\y->x*y)
pares n = map (\x->2*x) [1..n]

quadrado:: Num a => a->a
quadrado = (^2)
inverso = (1/)
metade x = shiftR x 1
dobro x = shiftL x 1
nDivisiveisPorM n m = [x | x<-[1..n], x `mod` m==0]
tabuada n = [n*x | x <-[0..9]]
indSup n m = [(x,y) | x<-[1..n], y<-[1..m], x>y]
prodCart xs ys = [x*y | x<-xs, y<-ys]
impares n = [x | x<-[1..2*n], odd x]
fatores n = [x | x<-[1..n], n `mod` x == 0]
primo y = fatores y == [1,y]
primos y = [x | x <- [1..y], fatores x == [1,x]]
perfeito x = sum (fatores (x))-x == x
primPErfeitos n = take n [x | x <-[2..], perfeito x]
fib x y = x:(fib y (x+y))

zipa:: [a] -> [b] -> [(a,b)]
zipa _ [] = []
zipa [] _ = []
zipa (x:xs) (y:ys) = (x,y):zipa xs ys

pairs (x:xs) = zipa (x:xs) xs
--ordenado:: [a]->[Bool]
ordenado xs = and [x <= y | (x, y)<-pairs xs]

prodEscalar:: (Num a)=> [a]->[a]->a
prodEscalar xs ys = sum [x*y | (x,y)<-zipa xs ys]

--nmrRepStr:: (Char a, Int b)=> [a]->b
nmrRepStr c xs = sum [1 | x<-xs, c==x]

idxsList x xs = [b |(a, b) <- zipa xs [0..] ,x == a]

pyths x = [(a,b,c) | a<-[1..x], b<-[1..x], c<-[1..x] , a^2 + b^2 == c^2]

maior x xs = [y | y<-xs, y<=x]
menor x xs = [y | y<-xs, y>x]
qSort [] = []
qSort (x:xs) = qSort (maior x xs) ++ [x] ++ qSort (menor x xs)

and'::[Bool]->Bool
and' xs = and [x | x<-xs]

concatena xss = [x| xs<-xss, x<-xs]
reply' s n = [s | x<-[1..n]]
slc x xs = [y | (y, t) <- zipa xs [1..], t==x]
slc' 0 (x:xs) = x
slc' x xs = slc' (x-1) xs
pertence x [] = False
pertence x [y] = x == y
pertence y (x:xs) = y==x || pertence x xs

merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = if x<y then
                    x:merge xs (y:ys)  else 
                    y:merge (x:xs) ys
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort (take w xs)) (mSort (drop w xs))
        where w = length xs `div` 2 

map' f xs = [f x | x<-xs]
map'' _ [] = []
map'' f (x:xs) = (f x):map f xs 

filter' _ [] = []
filter' f (x:xs) = if f x then x:(filter' f xs) else filter' f xs
filter'' f xs = [x | x<-xs, f x]

paraTodo f xs = and [f x | x<-xs]
paraTodo' _ [] = True
paraTodo' f (x:xs) = if f x then paraTodo' f xs else False
--paraTodo' f (x:xs) = f x && paraTodo' f xs

existe f xs = or [f x | x<-xs]
existe' _ [] = False
existe' f (x:xs) = if f x then True else existe' f xs
--existe' f (x:xs) = f x || existe' f xs

pegar' _ [] = []
pegar' f (x:xs)
        | f x = x:(pegar' f xs)
        | otherwise = pegar' f []

dropar _ [] = []
dropar f (x:xs)
        | f x = dropar f xs
        | otherwise = x:(dropar f xs)

odd' = not.even

segundo = head.tail
terceiro = head.tail.tail
ultimo = head.reverse
--foldRec::(a->b->b)->b->[a]->b
foldRec _ v [] = v
foldRec f v (x:xs) = f x (foldRec f v xs)
sum' = foldRec (+) 0
prod' = foldRec (*) 1
or' = foldRec (||) False
tam = foldRec (\_ count -> count + 1) 0
concat' = foldRec (++) []
inv = foldRec (\x xs-> xs++[x]) []

mSI::(Eq a)=> [a]->[a]->[a]
mSI [] maiorSeq = maiorSeq
mSI xs maiorSeq
        | length seqAtual > length maiorSeq = mSI resto seqAtual
        | otherwise = mSI resto maiorSeq
        where seqAtual = pegar' (==(head xs)) xs
              resto = dropar (==(head xs)) xs


mSI2 f [] maiorSeq = maiorSeq
mSI2 f (x:xs) maiorSeq
       | (length $ mayMSeq f (x:xs)) > length maiorSeq = mSI2 f xs (x:mayMSeq f (x:xs))
       | otherwise = mSI2 f xs maiorSeq
       where mayMSeq f (x:xs)
                | length (x:xs) == 1 = []
                | f x (head xs) = (head xs):(mayMSeq f xs)
                | otherwise = []
               