import Data.List.Split

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
                deriving Show

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
                putStrLn (show figures)
                --writeFile "areas.txt" figures
                where formatFigures hFigures = map words (splitOn "\n" hFigures)