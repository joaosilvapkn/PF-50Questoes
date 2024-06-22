--ex1
enumFromTo_ :: Int -> Int ->[Int]
enumFromTo_ x y | x > y = []
                | otherwise = x : enumFromTo_ (x+1) y


--ex2
enumFromThenTo_ :: Int -> Int-> Int -> [Int]
enumFromThenTo_ x inter y | x > y = []
                          | otherwise = x : enumFromThenTo_ (x+inter-1) inter y

--ex3
(+++) :: [a] -> [a] -> [a]
(+++) l1 l2 = l1 ++ l2

--ex4
(!!!) :: [a] -> Int -> a
(!!!) (h:t) n | n==0 = h 
              | otherwise = (!!!) t (n-1)

--ex5
reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ [x] = [x]
reverse_ (h:t) = (reverse_ t) ++ [h] 


--ex6
take_ :: Int -> [a] -> [a]
take_ n [] = []
take_ n (h:t) | n==0 = []
              | otherwise = h : take_ (n-1) t

--ex7
drop_ :: Int -> [a] -> [a]
drop_ n [] = []
drop_ n (h:t) | n==0 = (h:t) 
              | otherwise = drop_ (n-1) t

--ex8
zip_ :: [a] -> [b] -> [(a,b)]
zip_ [] [] = []
zip_ (h:t) [] = []
zip_ [] (h:t) = []
zip_ (h:t) (h2:t2) = (h,h2) : zip_ t t2

--ex9
replicate_ :: Int -> a ->[a]
replicate_ n x | n==0 = []
               | otherwise = x : replicate_ (n-1) x

--ex10
intersperse_ :: a -> [a] ->[a]
intersperse_ x [] = []
intersperse_ x [a] = [a]
intersperse_ x (a:b:t) = a : x : intersperse_ x (b:t)


--ex11
group_ :: Eq a => [a] -> [[a]]
group_ [] = []
group_ (h:t) = (h:takeWhile(==h) t) : group_ (dropWhile(==h) t) 

--ex12
concat_ :: [[a]] -> [a]
concat_ [] = []
concat_ (h:t) = h ++ concat_ t


--ex13
inits_ :: [a] -> [[a]]
inits_ [] = [[]]
inits_ l =  (inits_ (init l)) ++ [l]

--ex14
tails_ :: [a] -> [[a]]
tails_ [] = [[]]
tails_ l = [l] ++ (tails_ (tail l))

--ex15
heads_ :: [[a]] -> [a]
heads_ [] = []
heads_ ([] : t2) = heads_ t2
heads_ ((h:t) : t2) = h : heads_ t2

--ex16
total_ :: [[a]] -> Int
total_ [] = 0
total_ ([] : t2) = total_ t2
total_ ((h:t) : t2) = 1 + total_ ((t) : t2)


--ex17
fun_ :: [(a,b,c)] -> [(a,c)] 
fun_ [] = []
fun_ ((x,y,z) : t) = (x,z) : fun_ t

--ex18
cola_ :: [(String,b,c)] -> String
cola_ [] = []
cola_ ((x,y,z):t) = x ++ cola_ t

--ex19
idade_ :: Int -> Int -> [(String,Int)] -> [String]
idade_ ano num [] = []
idade_ ano num ((nome,nasc) : t) | nasc <= ano-num = [nome] ++ idade_ ano num t
                                 | otherwise = idade_ ano num t


--ex20
powerEnumFrom_ :: Int -> Int -> [Int]
powerEnumFrom_ n m | m==1 = [n^0]
                   | otherwise = (powerEnumFrom_ n (m-1)) ++ [n^(m-1)]

--ex21
isPrime_ :: Int -> Bool
isPrime_ n | length l==0 = True
           | otherwise = False

 where
 l = [m | m <- [2..floor(sqrt(fromIntegral(n)))], mod n m == 0]


--ex22
isPrefixOf_ :: Eq a => [a] -> [a] -> Bool
isPrefixOf_ [] l = True 
isPrefixOf_ l [] = False
isPrefixOf_ (h:t) (h2:t2) | h==h2 = isPrefixOf_ t t2 
                          | otherwise = False

--ex23
isSuffixOf_ :: Eq a => [a] -> [a] -> Bool
isSuffixOf_ [] l = True 
isSuffixOf_ l [] = False
isSuffixOf_ l1 l2 | last l1 == last l2 = isSuffixOf_ (init l1) (init l2)
                  | otherwise = False

--ex24
isSubsequenceOf_ :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf_ [] l = True
isSubsequenceOf_ l [] = False 
isSubsequenceOf_ (h:t) (h2:t2) | h==h2 = isSubsequenceOf_ t t2 
                               | otherwise = isSubsequenceOf_ (h:t) t2 


--ex25
elemIndices_ :: Eq a => a -> [a] -> [Int]
elemIndices_ x l = [n | n<-[0..(length l)-1], x==l !!! n]

--ex26
nub_ :: Eq a => [a] -> [a]
nub_ [] = []
nub_ (h:t) = h : nub_ (filter(/=h) t)

--ex27
delete_ :: Eq a => a -> [a] -> [a]
delete_ x [] = []
delete_ x (h:t) | x==h = t 
                | otherwise = h : delete_ x t

--ex28
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) [] [] = []
(\\\) [] l = []
(\\\) l [] = l 
(\\\) l (x:xs) = (\\\) (delete_ x l) xs


--ex29
union_ :: Eq a => [a] -> [a] -> [a]
union_ [] [] = []
union_ l [] = l
union_ [] l = l 
union_ l1 l2 | elem (last l2) l1 = union_ l1 (init l2) 
             | otherwise = (union_ l1 (init l2)) ++ [last l2]

--ex30
intersect_ :: Eq a => [a] -> [a] -> [a]
intersect_ [] [] = []
intersect_ l [] = []
intersect_ [] l = []
intersect_ (h:t) l | elem h l = h : intersect_ t l 
                   | otherwise = intersect_ t l

--ex31
insert_ :: Ord a => a -> [a] -> [a]
insert_ x [] = [x]
insert_ x (h:t) | x<h = x:h:t 
                | otherwise = h : insert_ x t 

--ex32
unwords_ :: [String] -> String
unwords_ [] = []
unwords_ [x] = x 
unwords_ (h:t) = h ++ " " ++ unwords_ t

--ex33
unlines_ :: [String] -> String
unlines_ [] = ""
unlines_ (h:t) = h ++ "\n" ++ unlines_ t

--ex34
pMaior_ :: Ord a => [a] -> Int
pMaior_ [] = -1
pMaior_ [x] = 0
pMaior_ (h:t) | h > (t !!! n) = 0 
              | otherwise = 1 + n

 where n = pMaior_ t 


--ex35
lookup_ :: Eq a => a -> [(a,b)] -> Maybe b
lookup_  x [] = Nothing 
lookup_  x ((y,z) : t) | x==y = Just z 
                       | otherwise = lookup_ x t

--ex36
preCrescente_ :: Ord a => [a] -> [a]
preCrescente_ [] = []
preCrescente_ (h:t) | length l1 > length l2 = l1 
                    | otherwise = l2
 where
  l1 = preCrescente_aux (h:t)
  l2 = preCrescente_ t

preCrescente_aux :: Ord a => [a] -> [a]
preCrescente_aux [] = []
preCrescente_aux [x] = [x]
preCrescente_aux (h:t) | h < head t = h : preCrescente_aux t 
                       | otherwise = [h]


--ex37
iSort_ :: Ord a => [a] -> [a]
iSort_ [] = []
iSort_ (h:t) = insert_ h (iSort_ t)

--ex38
menor_ :: String -> String -> Bool
menor_ [] [] = False
menor_ [] l = True 
menor_ l [] = False 
menor_ (h:t) (h2:t2) | h<h2 = True
                     | h==h2 = menor_ t t2 
                     | otherwise = False

--ex39
elemMSet_ :: Eq a => a -> [(a,Int)] -> Bool
elemMSet_ x [] = False 
elemMSet_ x ((y,n):t) | x==y = True 
                      | otherwise = elemMSet_ x t

--ex40
converteMSet_ :: [(a,Int)] -> [a]
converteMSet_ [] = []
converteMSet_ ((x,n):t) | n<=0 = converteMSet_ t 
                        | otherwise = x : converteMSet_ ((x,n-1):t)


--ex41
insereMSet_ :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet_ x [] = [(x,1)]
insereMSet_ x ((y,n):t) | x==y = ((y,n+1):t)
                        | otherwise = (y,n) : insereMSet_ x t

--ex42
removeMSet_ :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet_ x [] = []
removeMSet_ x ((y,n):t) | x==y = t
                        | otherwise = (y,n) : removeMSet_ x t

--ex43
constroiMSet_ :: Ord a => [a] -> [(a,Int)]
constroiMSet_ [] = []
constroiMSet_ (h:t) = (h, length (takeWhile (==h) t)+1) : (constroiMSet_ (dropWhile (==h) t)) 


--ex44
partitionEithers_ :: [Either a b] -> ([a], [b])
partitionEithers_ [] = ([], [])
partitionEithers_ (Left x : t) = juntaParesListas ([x], []) (partitionEithers_ t)
partitionEithers_ (Right x : t) = juntaParesListas ([], [x]) (partitionEithers_ t)

juntaParesListas :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
juntaParesListas (l1, l2) (l3, l4) = (l1 ++ l3, l2 ++ l4)


--ex45
catMaybes_ :: [Maybe a] -> [a]
catMaybes_ [] = []
catMaybes_ (Nothing : t) = catMaybes_ t 
catMaybes_ (Just x : t) = x : catMaybes_ t


data Movimento = Norte | Sul | Este | Oeste deriving Show

--ex46
caminho_ :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho_ (xi,yi) (xf,yf) | xi<xf = Este : caminho_ (xi+1,yi) (xf,yf)
                         | xi>xf = Oeste : caminho_ (xi-1,yi) (xf,yf)
                         | xi==xf && yi==yf = []  
                         | xi==xf && yi<yf = Norte : caminho_ (xi,yi+1) (xf,yf)
                         | xi==xf && yi>yf = Sul : caminho_ (xi,yi-1) (xf,yf)


--ex47
hasLoops_ :: (Int,Int) -> [Movimento] -> Bool
hasLoops_ (x,y) movs = hasLoops_aux (x,y) (x,y) movs

hasLoops_aux :: (Int, Int) -> (Int, Int) -> [Movimento] -> Bool
hasLoops_aux (x,y) (xcurr,ycurr) [] = False

hasLoops_aux (x,y) (xcurr,ycurr) (Este:t) | x==xcurr+1 && y==ycurr = True
                                          | otherwise = hasLoops_aux (x,y) (xcurr+1,ycurr) t

hasLoops_aux (x,y) (xcurr,ycurr) (Oeste:t) | x==xcurr-1 && y==ycurr = True
                                           | otherwise = hasLoops_aux (x,y) (xcurr-1,ycurr) t

hasLoops_aux (x,y) (xcurr,ycurr) (Norte:t) | x==xcurr && y==ycurr+1 = True
                                           | otherwise = hasLoops_aux (x,y) (xcurr,ycurr+1) t

hasLoops_aux (x,y) (xcurr,ycurr) (Sul:t) | x==xcurr && y==ycurr-1 = True
                                         | otherwise = hasLoops_aux (x,y) (xcurr,ycurr-1) t




type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

--ex48
contaQuadrados_ :: [Rectangulo] -> Int
contaQuadrados_ [] = 0
contaQuadrados_ (Rect (x1,y1) (x2,y2) : t) | x1>x2 && y1>y2 && sqrt((x1-x1)^2 + (y1-y2)^2)==sqrt((x1-x2)^2 + (y2-y2)^2) = 1 + contaQuadrados_ t
                                           | x1>x2 && y1>y2 && sqrt((x1-x1)^2 + (y1-y2)^2)/=sqrt((x1-x2)^2 + (y2-y2)^2) = contaQuadrados_ t

                                           | x1>x2 && y1<y2 && sqrt((x1-x1)^2 + (y2-y1)^2)==sqrt((x1-x2)^2 + (y1-y1)^2) = 1 + contaQuadrados_ t
                                           | x1>x2 && y1<y2 && sqrt((x1-x1)^2 + (y2-y1)^2)/=sqrt((x1-x2)^2 + (y1-y1)^2) = contaQuadrados_ t 

                                           | x1<x2 && y1>y2 && sqrt((x1-x1)^2 + (y1-y2)^2)==sqrt((x2-x1)^2 + (y2-y2)^2) = 1 + contaQuadrados_ t
                                           | x1<x2 && y1>y2 && sqrt((x1-x1)^2 + (y1-y2)^2)/=sqrt((x2-x1)^2 + (y2-y2)^2) = contaQuadrados_ t

                                           | x1<x2 && y1<y2 && sqrt((x2-x2)^2 + (y2-y1)^2)==sqrt((x2-x1)^2 + (y1-y1)^2) = 1 + contaQuadrados_ t
                                           | x1<x2 && y1<y2 && sqrt((x2-x2)^2 + (y2-y1)^2)/=sqrt((x2-x1)^2 + (y1-y1)^2) = 1 + contaQuadrados_ t


--ex49
areaTotal_ :: [Rectangulo] -> Float
areaTotal_ [] = 0
areaTotal_ (Rect (x1,y1) (x2,y2) : t) | x1>x2 && y1>y2 = (sqrt((x1-x1)^2 + (y1-y2)^2) * sqrt((x1-x2)^2 + (y2-y2)^2)) + areaTotal_ t
                                      | x1>x2 && y1<y2 = (sqrt((x1-x1)^2 + (y2-y1)^2) * sqrt((x1-x2)^2 + (y1-y1)^2)) + areaTotal_ t
                                      | x1<x2 && y1>y2 = (sqrt((x1-x1)^2 + (y1-y2)^2) * sqrt((x2-x1)^2 + (y2-y2)^2)) + areaTotal_ t
                                      | x1<x2 && y1<y2 = (sqrt((x2-x2)^2 + (y2-y1)^2) * sqrt((x2-x1)^2 + (y1-y1)^2)) + areaTotal_ t



data Equipamento = Bom | Razoavel | Avariado deriving Show

--ex50
naoReparar_ :: [Equipamento] -> Int 
naoReparar_ [] = 0
naoReparar_ (Bom : t) = 1+naoReparar_ t 
naoReparar_ (Razoavel : t) = 1+naoReparar_ t 
naoReparar_ (Avariado : t) = naoReparar_ t
