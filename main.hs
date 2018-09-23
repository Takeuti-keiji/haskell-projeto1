main = do
    contents <- getLines  --le a entrada como um bloco
    let b = parseLines contents       -- cada linha uma string em uma lista      
    let c = parseWords(head  b)       -- cada linha uma lista de string
    let a = (encontra (head $ head $ c) c) 
    let d = mapedTail a 
    let e = unico $ mapedHead c -- lista com o primeiro elemento de cada sublista, sem repeticao
    putStr(" \n \n")
    print(c)
    putStr(" \n \n")
    print(a)
    putStr(" \n \n")
    print(d)
    putStr(" \n \n")
    print(e)


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

type Edge = (Node,Meio) 
type Node = String
type Meio = [(String,Weight)]
type Weight = Double
type Graph = [(Node, [Edge])]







unico [] = [] 
unico (x:xs)
    |elem x xs = unico xs
    |otherwise = x:unico xs