data Arv w = Nil | No (Arv w) w (Arv w) deriving (Show)

inserir b Nil = No Nil b Nil
inserir b (No rm w rM)
            | b == w = No rm w rM
            | b < w = No (inserir b rm) w rM
            | b > w = No rm w (inserir b rM)

criar [] = Nil
criar (x:xs) = criarAux (No Nil x Nil) xs
               where criarAux arv [] = arv
                     criarAux arv (l:ls) = criarAux (inserir l arv) ls

buscar Nil = []
buscar (No rm w rM) = (buscar rm) ++ [w] ++ (buscar rM)

instance Functor Arv where
    fmap f (Nil) = Nil
    fmap f (No rm w rM) = (No (fmap f rm) (f w) (fmap f rM))

type No = Int
type Adj b = [(b, No)]
type Contexto a b = (Adj b, No, a, Adj b)
data Grafo a b = Vazio | Conectado (Contexto a b) (Grafo a b) deriving (Show)
g = Conectado ([("yyy",2)],1,'a',[("xxx",2),("zzz",3)]) (Conectado ([("xxx",1),("www",3)],2,'b',[("yyy",1)]) (Conectado ([("zzz",1)],3,'c',[("www",2)]) Vazio))

