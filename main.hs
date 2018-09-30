
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

menorCaminho :: Graph ->Pqueue -> Nome -> Nome -> Pqueue
menorCaminho g pq o d = menorCaminho' g pq [] o "a-pe" d

menorCaminho' :: Graph -> Pqueue -> [Nome] -> Nome -> Nome -> Nome -> Pqueue
menorCaminho' g pq v o m d 
    |o == d = pq
    |otherwise =
        let pq' =  checaDist (snd (achaNoPartida o g)) pq m 
            v' = o':v 
            o' =fst t
            m' = snd t
            t = menorDist pq'
        in menorCaminho' g pq' v' o' m' d
        
menorDist :: Pqueue -> (Nome,Nome)
menorDist pq = menorDist' pq ("","")
menorDist' (x:xs) ("","") = menorDist' xs (fst x,snd(snd(snd x)))
menorDist' (x:xs) w
    |fst(snd x) <  snd w =  menorDist' xs (fst x,snd(snd(snd x)))
    |otherwise = menorDist' xs w 
        
checaDist :: [Edge] -> Pqueue ->  Nome -> Pqueue   
checaDist [] pq _ _= pq
checaDist (x:xs) pq m 
    |a >= b && b >= 0 = checaDist xs pq m 
    |otherwise =  checaDist xs (atualizaPq x pq a m ) m 
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
    |fst e == fst x = (fst e,(snd (snd e),(v,m))) ++ atualizaPq e xs w
    |otherwise = x ++ atualizaPq e xs w

dist :: [Meio] -> Pqueue -> Nome -> Pqueue
dist e pq m o= dist' e pq m  (m,0)
dist' [] pq m  r = r
dist' (x:xs) pq m  (_,0)
    |fst x == "a-pe" = dist' xs pq m x
    | m /= fst x = dist' xs pq m  (fst x,((snd x) + esperaBus fst x ))
    |otherwise = dist' xs pq m x
dist' (x:xs) pq m r
    |fst x == "a-pe" = dist' xs pq m a 
    | m /= fst x = dist' xs pq m b
    |otherwise = dist' xs pq m a
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
    |otherwise = esperaBus xs s
    
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








