import System.IO  
import Control.Monad

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
    print(g)
    print(head(tail (tail (tail (tail g)))))
    
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
        
             
preenche'' :: [String]->[[String]] -> [Edge]->[Edge] 
preencheEdge :: [String] -> [[String]] -> [Edge]    
preencheEdge n l = preenche'' n l [] 
preenche'' [] l e = e
preenche'' (x:xs) l e = e ++ preenche'' xs l [(x,m)] 
    where m = (map (\[a,b]-> (a,(read(b)::Double))) b)
          b = (mapedTail) c
          c = (encontra x l)
          


type Edge = (Node,Meio) 
type Node = String
type Meio = [(String,Weight)]
type Weight = Double
type Graph = [(Node, [Edge])]







