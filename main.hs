
main = do
    contents <- getLines  --le a entrada como um bloco
    let b = parseLines contents       -- cada linha uma string em uma lista      
    let c = parseWords(head  b)       -- cada linha uma lista de string
    let a = (encontra (head $ head $ c) c) 
    let d = mapedTail a 
    let e = unico $ mapedHead c -- lista com o primeiro elemento de cada sublista, sem repeticao
    let f = unico $ mapedHead d
    let l = parseWords(head(tail b))
    let k = parseWords(head(tail(tail b)))
    let g = preencheGraph e c    
    let h = criaPqueue e "f"
    let r = menorCaminho g h "a" "f" l
    print(k)
    putStr(" \n \n") 
    print(g)
    putStr(" \n \n")
    print(h)
    putStr(" \n \n") 
    print(r)
    print(percorrePqueue r "f")
   
    
getLines :: IO [String]
getLines = lines <$> getContents

parseLines :: [String] -> [[String]]
parseLines []= []
parseLines s 
    |b == [] = [a] ++ parseLines b 
    |otherwise = [a] ++ (parseLines  (tail  b)  )
    where a = takeWhile (/= "") s
          b = (dropWhile (/= "") s)

parseWords a = map words a      
mapedHead a = (map head) a
mapedTail a = (map tail) a

encontra a b =  (filter ((\y (x:xs) -> y == x) a) ) b

unico [] = [] 
unico (x:xs)
    |elem x xs = unico xs
    |otherwise = x:unico xs
    
preencheGraph :: [String] -> [[String]] -> Graph    
preencheGraph n l = preenche' n l []
preenche' [] l g = g
preenche' (x:xs) l g = g ++ preenche' xs l [(x,preencheEdge a b )] 
    where a = unico $ mapedHead b
          b = mapedTail c
          c = (encontra x l)
        
             
preencheEdge :: [String] -> [[String]] -> [Edge]    
preencheEdge n l = preenche'' n l []

preenche'' :: [String]->[[String]] -> [Edge]->[Edge] 
preenche'' [] l e = e
preenche'' (x:xs) l e = e ++ preenche'' xs l [(x,m)] 
    where m = (map (\[a,b]-> (a,(read(b)::Double))) b)
          b = (mapedTail) c
          c = (encontra x l)

achaNoPartida :: String->Graph->Node
achaNoPartida _ [] = ("",[])
achaNoPartida n (h:t)
          | n == (nomeNo h) = h
          | otherwise = achaNoPartida n t
          
achaEdge :: String -> [Edge] -> [Meio]
achaEdge _ [] = []
achaEdge n (h:t)
    |n == (nomeNo h) = snd h
    |otherwise = achaEdge n t
    
nomeNo :: (a,b)-> a
nomeNo n = fst n

criaPqueue :: [String] -> String -> Pqueue          --cria a fila de prioridade
criaPqueue [] _ = []
criaPqueue (x:xs) v 
    |x == v = (x,(0,("",""))):criaPqueue xs v
    |otherwise = (x,(-1,("",""))):criaPqueue xs v

menorCaminho :: Graph ->Pqueue -> Nome -> Nome ->[[String]]-> Pqueue     
menorCaminho g pq o d l = menorCaminho' g pq [] o "a-pe" d l

menorCaminho' :: Graph -> Pqueue -> [Nome] -> Nome -> Nome -> Nome -> [[String]] -> Pqueue    -- acha o menor caminho recebe o grafo, a lista de prioridade inicializada, lista de visitados, um no , meio de transporte ate esse no e o destino
menorCaminho' g pq v o m d l
    |o == d = pq
    |otherwise =
        let pq' =  checaDist (snd (achaNoPartida o g)) pq m o l--atualiza a Pqueue com os vizinhos de o
            v' = ([o'] ++ v)  --adiciona o aos visitados
            o' =fst t  -- nome do menor no
            m' = snd (snd(snd t)) --meio do menor no
            t = menorDist pq' v -- acha o proximo no com menor distancia
        in menorCaminho' g pq' v' o' m' d l
               
menorDist :: Pqueue -> [Nome] -> (Nome,Spec)                      --acha o menor no nao visitado na fila de prioridade
menorDist pq v = menorDist' pq v ("",(0,("","")))

menorDist' :: Pqueue -> [Nome] -> (Nome,Spec) -> (Nome,Spec) 
menorDist' [] _ w = w
menorDist' (x:xs) v ("",(0,("",""))) 
    |(notElem (fst x) v) = menorDist' xs v x
    |otherwise = menorDist' xs v ("",(0,("","")))
        
menorDist' (x:xs) v w
    |(fst(snd x) <  fst(snd w)) && (notElem (fst x) v) =  menorDist' xs v x
    |otherwise = menorDist' xs v w 
        
checaDist :: [Edge] -> Pqueue ->  Nome -> String -> [[String]] -> Pqueue         --atualiza a Pqueue com os vizinhos 
checaDist [] pq _  _ _= pq
checaDist (x:xs) pq m o l
    |a >= b && b >= 0 = checaDist xs pq m o l
    |otherwise =  checaDist xs (atualizaPq x pq a m o) m o l
    where   
        a = snd(dist (snd x) pq m l) + (distOrigem (fst x) pq)
        b =  fst(snd(achaNo (fst x) pq))
        
achaNo _ [] = ("",(0,("","")))
achaNo n (h:t)
          | n == (nomeNo h) = h
          | otherwise = achaNo n t
          
          
atualizaPq :: Edge -> Pqueue -> Weight -> String -> String -> Pqueue
atualizaPq _ [] _ _ _= []
atualizaPq e (x:xs) w m o 
    |fst e == fst x = [(fst e,(w,(o,m)))] ++ atualizaPq e xs w m o
    |otherwise = [x] ++ atualizaPq e xs w m o

dist' :: [Meio] -> Pqueue -> Nome -> (Nome,Weight) -> [[String]] -> (Nome,Weight)
dist :: [Meio] -> Pqueue -> Nome  -> [[String]] -> (Nome,Weight)
dist e pq m l = dist' e pq m  (m,0) l
dist' [] pq m  r _ = r
dist' (x:xs) pq m  (_,0) l
    |fst x == "a-pe" = dist' xs pq m x l
    | m /= (fst x) = dist' xs pq m  (fst x,((snd x) + esperaBus l (fst x))) l
    |otherwise = dist' xs pq m x l
dist' (x:xs) pq m r l
    |fst x == "a-pe" = dist' xs pq m a l
    | m /= (fst x) = dist' xs pq m b l
    |otherwise = dist' xs pq m a l
    where
        a =maisRapido r x
        b =maisRapido r (fst x,((snd x) + (esperaBus l (fst x))))
        
maisRapido :: Meio -> Meio -> Meio
maisRapido a b  
    |snd a < snd b = a
    |otherwise = b
    
esperaBus :: [[String]] -> String -> Double
esperaBus [] _ = 0
esperaBus (x:xs) s
    |(head x) == s = ((read (head(tail x)) :: Double )/2)
    |otherwise = esperaBus xs s
    
distOrigem :: Nome -> Pqueue -> Weight
distOrigem n pq = (fst(snd(achaNo n pq)))

percorrePqueue :: Pqueue -> String -> String      --nao implementada
percorrePqueue pq v = percorrePqueue' pq pq v ""
percorrePqueue' pq (x:xs) v s 
    |(fst x) == v = percorrePqueue' pq  pq (fst $ snd $ snd x) (s ++ (snd $ snd $ snd x )++ (fst x))
    |otherwise = percorrePqueue' pq xs v s
     
type Graph = [Node]
type Node =(Nome, [Edge])
type Edge = (Nome,[Meio]) 
type Meio = (Nome,Weight)
type Weight = Double
type Nome = String

type Pqueue  = [(Nome,Spec)]     --priority queue nome do nó e suas
type Spec = (Weight,(Nome,Nome))  --especificaçoes peso, nó de origem e modo Transporte








