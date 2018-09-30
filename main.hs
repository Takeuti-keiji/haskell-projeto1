
main = do
    contents <- getLines  --le a entrada como um bloco
    let b = parseLines contents       -- cada linha uma string em uma lista      
    let c = parseWords(head  b)       -- cada linha uma lista de string
    let a = (encontra (head $ head $ c) c) 
    let d = mapedTail a 
    let e = unico $ mapedHead c -- lista com o primeiro elemento de cada sublista, sem repeticao
    let f = unico $ mapedHead d
    putStr(" \n \n")    
    let g = preencheGraph e c    
    putStr(" \n \n")
    mapM_  print(g)
    putStrLn "\tEdges:"
    mapM_ print(map snd g)
    putStrLn "\n"
    print(achaNoPartida "f" g)
    
    print(criaPqueue e "a")
    
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

criaPqueue :: [String] -> String -> Pqueue
criaPqueue [] _ = []
criaPqueue (x:xs) v 
    |x == v = (x,(0,("",""))):criaPqueue xs v
    |otherwise = (x,(-1,("",""))):criaPqueue xs v

menorCaminho :: Graph ->Pqueue -> [Nome] -> Nome -> Nome -> (Pqueue,[Nome])
menorCaminho g pq o d = menorCaminho'g pq [] [] o "a-pe" d

menorCaminho' :: Graph -> Pqueue -> [Nome] -> [Nome] -> Nome -> Nome -> Nome -> (Pqueue,[Nome])
menorCaminho' g pq v t o m d 
    |v == d = (pq,t)
    |otherwise =
        let pq' =  checaDist (snd achaNoPartida o g) pq m o
            v'
            t'
            o'
            m'
        in menorCaminho' g pq' v' t' o' m' d
        
checaDist :: [Edge] -> Pqueue ->  Nome -> Nome-> Pqueue   
checaDist [] pq _ _= pq
checaDist (x:xs) pq m o 
    |a >= b && b > 0 = checaDist xs pq m o
    |otherwise =  checaDist xs (atualizaPq x pq a m ) m o
    where   
        a = (dist (snd x) pq m) + (distOrigem fst x pq)
        b =  fst (snd(achaNo (nomeNo x) pq))
        
achaNo _ [] = ("",("",""))
achaNo n (h:t)
          | n == (nomeNo h) = h
          | otherwise = achaNo n t
          
          
atualizaPq :: Edge -> Pqueue -> Weight -> Pqueue
atualizaPq _ [] _ = []
atualizaPq e (x:xs) w 
    |fst e == fst x = (fst e,(snd (snd e),(v,m))) ++ atualizaPq
    |otherwise = x ++ atualizaPq 

dist :: [Meio] -> Pqueue -> Nome -> Pqueue
dist e pq m o= dist' e pq m  (m,0)
dist' [] pq m  r = r
dist' (x:xs) pq m  (_,0) 
    |fst x == "a-pe" = dist' xs pq m x
    |otherwise = if m /= fst x dist' xs pq m  (fst x,((snd x) + esperaBus fst x ))
                    else
                    dist' xs pq m x
dist' (x:xs) pq m r
    |fst x == "a-pe" dist' xs pq m a 
    |otherwise = if (m /= fst x) dist' xs pq m b
    else dist' xs pq m a
    
    where
        a =maisRapido r x
        b =maisRapido r (fst x,((snd x) + esperaBus fst x ))
        
maisRapido :: Meio->Meio->Meio
maisRapido a b  
    |snd a < snd b = a
    |otherwise = b
    
esperaBus :: [[String]] -> String -> Double
esperaBus (x:xs) s
    |fst x == s = ((snd x)/2)
    |otherwise esperaBus xs s
    
distOrigem :: Nome -> Pqueue -> Weight
distOrigem n pq = (fst(snd(achaNo n pq)))
     
type Graph = [Node]
type Node =(Nome, [Edge])
type Edge = (Nome,[Meio]) 
type Meio = (Nome,Weight)
type Weight = Double
type Nome = String

type Pqueue  = [(Nome,Spec)]     --priority queue nome do nó e suas
type Spec = (Weight,(Nome,Nome))  --especificaçoes peso, nó de origem e modo Transporte








