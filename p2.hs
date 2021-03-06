import Data.List
type Pos = (Float, Float)
origem :: Pos
origem = (0,0)
esquerda :: Pos -> Pos
esquerda (x,y) = (x-1,y)
cima (x,y) = (x,y+1)
dist::Pos->Pos->Float
dist (x1,y1) (x2,y2) = sqrt(((x2-x1)^2+(y2-y1)^2))


type Pos1 = Pos
origem1 = origem
esquerda1 = esquerda

type PosP a = (a, a, a)
vol :: PosP Float -> Float
vol (x,y,z) = x*y*z

data Resp = Sim | Nao | Desconhecido
            deriving (Show, Eq)

type Reps = [Resp]

chaveia Sim = Nao
chaveia Nao = Sim
chaveia _ = Desconhecido

data Figure = Ret (Pos,Pos)
                | Circ (Pos, Float)
                | Tri (Pos, Pos, Pos)
                deriving Show

type Figures = [Figure]

square :: (Float, Float) -> Figure
square (x,y) = Ret ((x,x),(y,y))

area (Ret ((x1,y1),(x2,y2))) = (x2-x1)*(y2-y1)
area (Circ ((_,_), r)) = pi*r*r
area (Tri ((x1,y1),(x2,y2),(x3,y3))) = (x1*y2 + y1*x3 + x2*y3 - (x3*y2 + y3*x1 + x2*y1))/(fromIntegral 2)

mover (Circ ((x,y), r)) mx my = (Circ ((x+mx,y+my), r))
mover (Ret ((x1,y1),(x2,y2))) mx my = (Ret ((x1+mx,y1+my),(x2+mx,y2+my)))
mover (Tri ((x1,y1),(x2,y2),(x3,y3))) mx my = (Tri ((x1+mx,y1+my),(x2+mx,y2+my), (x3+mx,y3+my)))


data Talvez a = Nada
                | Apenas a
                deriving (Show, Eq)

cabeca :: [a] -> Talvez a
cabeca [] = Nada
cabeca (a:as) = Apenas a

div' :: Integral a => a->a->Talvez a
div' a 0 = Nada
div' a b = Apenas (a`div`b)

find' :: Eq a => [a] -> a -> Talvez a
find' [] y = Nada
find' (x:xs) y
            | x == y = Apenas x
            | otherwise = find' xs y

lerFiguras = do hFigures <- readFile "figures.txt" 
                let figures = formatFigures hFigures
                writeFile "areas.txt" figures
                where formatFigures hFigures =  calcArea $ toFigure $ map words (lines hFigures)

readFloat :: String -> Float
readFloat = read

toFigure :: [[[Char]]] -> Figures
toFigure [] = []
toFigure (fig:figs)
            | head fig == "Circ" = (Circ ((readFloat (fig!!1), readFloat (fig!!2)),readFloat (fig!!3))):(toFigure figs)
            | head fig == "Ret" = (Ret ((readFloat (fig!!1), readFloat (fig!!2)), (readFloat (fig!!3), readFloat (fig!!4)))):(toFigure figs)
            | head fig == "Tri" = (Tri ((readFloat (fig!!1), readFloat (fig!!2)), (readFloat (fig!!3), readFloat (fig!!4)), (readFloat (fig!!5), readFloat (fig!!6)))):(toFigure figs)
            | otherwise = toFigure figs

calcArea [] = "\n"
calcArea (fig:figs) = toString fig ++ "\n" ++ (calcArea figs)

toString (Ret ((x1,y1),(x2,y2))) = "Ret " ++ (show ((x1,y1),(x2,y2))) ++ " area = " ++ (show $ area (Ret ((x1,y1),(x2,y2))))
toString (Tri ((x1,y1),(x2,y2),(x3,y3))) = "Tri " ++ (show ((x1,y1),(x2,y2),(x3,y3))) ++ " area = " ++ (show $ area (Tri ((x1,y1),(x2,y2),(x3,y3))))
toString (Circ ((x,y), r)) = "Circ " ++ (show ((x,y), r)) ++ " area = " ++ (show $ area (Circ ((x,y), r)))

data Nat = Zero | Succ Nat
          deriving Show

intNat :: Int -> Nat
intNat 0 = Zero
intNat n = Succ (intNat (n-1))

nat2Int :: Nat->Int
nat2Int Zero = 0
nat2Int (Succ nat) = 1 + (nat2Int nat)

add :: Nat -> Nat -> Int
add Zero Zero = 0
add (Succ m) Zero = 1 + (add m Zero)
add Zero (Succ n) = 1 + (add Zero n)
add (Succ m) (Succ n) = 2 + (add m n)

mult :: Nat -> Nat -> Int
mult Zero _ = 0
mult (Succ m) n = (nat2Int n) + (mult m n)

data Expr = Val Int | Add Expr Expr | Mul Expr Expr
            deriving Show

eval (Val h) = h
eval (Add h o) = eval h + eval o
eval (Mul h o) = eval h * eval o

{- class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
 -}

instance Eq Figure where
  (Ret (p1,p2)) == (Ret (p3,p4)) =  area (Ret (p1,p2)) == area (Ret (p3,p4))
  (Circ (p1,f)) == (Circ (p2, r)) = f == r
  (Tri (p1,p2,p3)) == (Tri (p4,p5,p6)) = area (Tri (p1,p2,p3)) == area (Tri (p4,p5,p6))

instance Eq Expr where
  (Val o) == (Val b) = o == b
  (Add e1 e2) == (Add e3 e4) = (eval (Add e1 e2)) == (eval (Add e3 e4))
  (Mul e1 e2) == (Mul e3 e4) = (eval (Mul e1 e2)) == (eval (Mul e3 e4))

data Arv a = Nil
          | No (Arv a) a (Arv a)
          deriving (Show, Eq)

findArv Nil _ = Nada
findArv (No ra w rb) f
                  | w == f = Apenas f
                  | findArv ra f /= Nada || findArv rb f /= Nada = Apenas f
                  | otherwise = Nada

vazia Nil = True
vazia _ = False

findArvBin Nil _ = Nada
findArvBin (No ra w rb) f
                  | w == f = Apenas f
                  | f>w = findArvBin rb f
                  | otherwise = findArvBin ra f

addToArvBin Nil f = No Nil f Nil
addToArvBin (No ra w rb) f
                  | f >= w = No ra w (addToArvBin rb f)
                  | otherwise = No (addToArvBin ra f) w rb

inserirRamo Nil Nil = Nil
inserirRamo Nil (No rq p re) = (No rq p re) 
inserirRamo (No ra w rb) (No rq p re) = No ra w (inserirRamo rb (No rq p re))
 
remove Nil f = Nil
remove (No ra w rb) f
                  | f == w = (inserirRamo ra rb)
                  | otherwise = (No (remove ra f) w (remove rb f)) 

insere Nil f = No Nil f Nil
insere (No ra w rb) f
              | w == f = (No ra w rb)
              | f>w = (No ra w (insere rb f))
              | otherwise = (No (insere ra f) w rb)

emOrdem Nil = []
emOrdem (No ra w rb) = (emOrdem ra) ++ [w] ++ (emOrdem rb)

preOrdem Nil = []
preOrdem (No ra w rb) = [w] ++ (preOrdem ra) ++ (preOrdem rb)

posOrdem Nil = []
posOrdem (No ra w rb) = (posOrdem ra) ++ (posOrdem rb) ++ [w]

lista2Arv [x] = No Nil x Nil
lista2Arv xs = (No (lista2Arv (esquerda sorted)) (central (sorted)) (lista2Arv (direita sorted)))
            where sorted = sort xs
                  central xs = xs!!((length xs)`div`2)
                  esquerda xs = take ((length xs)`div`2) xs
                  direita xs = drop (((length xs)`div`2)+1) xs

data ArvN a = NilN
              | NoN a [ArvN a]
              deriving Show

readInt :: String -> Int
readInt = read

lerArvN = do hArvN <- readFile "arvn.txt"
             let arvN =  (map (map readInt) (formatArvN hArvN))
             let b = arvNGen (head arvN) (tail arvN)
             writeFile "Oarvn.txt" (show b)
             where formatArvN a = map words (lines a)

arvNGen  = NoN (head xs) [(arvNGenf (tail xs) xss)] 



arvNGenf [] _ arv = arv
arvNGenf (f:fs) (xs:xss) arv
                | f == head xs = (NoN f [(toArvN (tail xs))])
                | otherwise = arvNGenf fs xss (NoN f [NilN])
                where toArvN [x] = NoN x [NilN]
                      toArvN (x:xs) = NoN x [(toArvN xs)]