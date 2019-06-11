module Bomber where
import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import Data.List

type Size = Int
type CFloor = [(Char,Int,Int)]
type Floor =  [String]


-- Funçao que reconstroi um mapa atraves de um mapa de triplos
fromTriple :: CFloor->Int->Int->Floor
fromTriple mapa lin tam | lin<tam = new_mapa : fromTriple mapa (lin+1) tam
                        | otherwise = []
                   where
                   fnew_mapa = filter (\(a,b,c)-> c == lin) mapa 
                   new_mapa = map (\(a,b,c)-> a) fnew_mapa

-- Funçao que cria um mapa de triplos (Char,Int,Int)
toTriple :: Floor->Int->Int->CFloor
toTriple [] _ _= []
toTriple (h:t) l size = res ++ toTriple t (l+1) size
            where
                col = enumFromThenTo 0 1 size
                lin = replicate size l
                res = zip3 h col lin

-- Funçao que separa o mapa das posiçoes dos jogadores, bombas e power-ups
toCFloor :: [String]->CFloor
toCFloor mapa = b
            where
                a = filter (\a-> head a == '#') mapa
                b = toTriple a 0 (length a)

-- Funçao que remove o carater numa dada posiçao 
remPanel :: Int -> Int-> CFloor ->CFloor
remPanel x y mapa = filter (\(a,b,c)-> b/=x || c/=y) mapa 


-- Funçao que coloca blocos no mapa
blockMap :: CFloor->Int->Int->Int->Int->Int->Int->CFloor
blockMap mapa _ _ 0 _ _ _ = mapa
blockMap mapa x y blocos size tam dir |dir == 0 && blocos >0 && size >0 = blockMap new_mapa (x+1) y (blocos-1) (size-1) tam dir
                                      |size == 0 && dir == 0 = blockMap mapa (x-1) (y+1) blocos (tam-1) (tam-1) 1
                                      |dir == 1 && blocos >0 && size >0 = blockMap new_mapa x (y+1) (blocos-1) (size-1) tam dir
                                      |size == 0 && dir == 1 = blockMap mapa (x-1) (y-1) blocos tam tam 2
                                      |dir == 2 && blocos >0 && size >0 = blockMap new_mapa (x-1) y (blocos-1) (size-1) tam dir
                                      |size == 0 && dir == 2 = blockMap mapa (x+1) (y-1) blocos (tam-1) (tam-1) 3
                                      |dir == 3 && blocos >0 && size >0 = blockMap new_mapa x (y-1) (blocos-1) (size-1) tam dir
                                      |size == 0 && dir == 3 = blockMap mapa (x+1) (y+1) blocos tam tam 0
                                      where
                                      rem_mapa = (remPanel x y mapa)
                                      new_mapa = (insert ('#',x,y) rem_mapa)

-- Criterio de Ordenação para triplos CFloor
crit (a,b,c) (d,e,f)  | b == e && c < f = LT
                      | b < e = LT
                      | otherwise = GT

-- Funçao que destroi o mapa
mBlockMap :: [String]->Int->CFloor
mBlockMap mapa time | blocos>0 = bMap 
                    | otherwise = cmapa
    where
        bMap = (sortBy crit (blockMap cmapa 1 1 blocos tam tam dir))
        cmapa = toCFloor mapa
        tam = (length $ head mapa) - 2
        blocos = tam*tam + 1 - time -- ((dim-2)^2)+1 -time 
        dir = 0

aux3PU :: String -> [Int]->(String,Int,Int)
aux3PU s [x,y] = (s,x,y)

to3PU:: [String]->(String,Int,Int)
to3PU s = aux3PU hs ts
    where
    hs = head s
    ts = map (\a -> read a ::Int) (tail s)

tPUstr :: [(String,Int,Int)]->[String]
tPUstr info = map (\(a,b,c)-> a++" "++show b ++ " " ++ show c) info

toPosPU::[String]->[(String,Int,Int)]
toPosPU info = newt
             where
             pos = (filter (\a-> head a == '!' || head a == '+') info)
             npos = map words pos
             newt = map to3PU npos


tBstr ::[(String,Int,Int,Int,Int,Int)]->[String]
tBstr info = map (\(a,b,c,d,e,f)->a++" "++show b ++ " " ++ show c ++ " "++ show d ++ " " ++ show e ++ " "++ show f  ) info

aux6B :: String->[Int]->(String,Int,Int,Int,Int,Int)
aux6B s [a,b,c,d,e] = (s,a,b,c,d,e)

to6B :: [String]->(String,Int,Int,Int,Int,Int)
to6B info = aux6B h t
    where
    h = head info
    t = map (\a -> read a ::Int) (tail info)

toPosB::[String]->[(String,Int,Int,Int,Int,Int)]
toPosB mapa = newt
    where
    pos = (filter (\a-> head a == '*') mapa)
    npos = map words pos
    newt = map to6B npos


aux4P :: String->[Int]->String->(String,Int,Int,String)
aux4P p [x,y] pu  = (p,x,y,pu)


to4P :: [String]->(String,Int,Int,String)
to4P info = aux4P hs pos pu
    where
    hs = head info
    pos = map (\a -> read a ::Int) (take 2 (tail info))
    pu = last info

tPstr::[(String,Int,Int,String)]->[String]
tPstr info = map (\(a,b,c,d)-> a++" "++show b ++ " " ++ show c ++ " "++d ) info

toPosP::[String]->[(String,Int,Int,String)]
toPosP mapa = newt
    where
     pos = (filter (\a-> head a == '0' || head a == '1'|| head a == '2'|| head a == '3') mapa)
     npos = map words pos
     newt = map to4P npos


-- Funçao que remove os jogadores devido á destruiçao do mapa
rempmap::CFloor->[(String,Int,Int,String)]->[(String,Int,Int,String)]
rempmap [] l = l
rempmap l [] = []
rempmap ((a,x,y):t) jog |a == '#' = rempmap t njog
                        |otherwise = rempmap t jog
    where
    njog = filter (\(e,r,t,q)-> r/=x || t /=y) jog




-- Funçao que remove as bombas devido á destruiçao do mapa
rembmap::CFloor->[(String,Int,Int,Int,Int,Int)]->[(String,Int,Int,Int,Int,Int)]
rembmap [] l = l
rembmap l [] = []
rembmap ((a,x,y):t) b |a == '#' = rembmap t nbomb
                      |otherwise = rembmap t b
    where
    nbomb = filter (\(e,r,t,q,u,i)-> r/=x || t /=y) b

-- Funçao que remove os power ups devido á destruiçao do mapa
rempumap ::CFloor->[(String,Int,Int)]->[(String,Int,Int)]
rempumap [] l = l
rempumap l [] = []
rempumap ((a,x,y):t) pu | a == '#' = rempumap t npu
                        | otherwise = rempumap t pu
    where
    npu = filter (\(e,r,t)-> r/=x || t /=y) pu
-- Funçao que diz se a coordenada de explosao é valida
getPosD :: Int->Int->Int->[String]->[(Int,Int)]
getPosD x y r mapa | r>0 && ((!!) ((!!) mapa y) x) == ' ' = (x,y) : getPosD (x+1) y (r-1) mapa
                   | r>0 && ((!!) ((!!) mapa y) x) == '?' = (x,y) : []
                   | otherwise = []

-- Funçao que nos dá as coordenadas explodidas numa direçao
getExpPanelD:: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanelD [] mapa = []
getExpPanelD ((x,y,r):t) mapa = (getPosD (x+1) y r mapa) ++ (getExpPanelD t mapa)


-- Funçao que diz se a coordenada de explosao é valida
getPosE :: Int->Int->Int->[String]->[(Int,Int)]
getPosE x y r mapa | r>0 && ((!!) ((!!) mapa y) x) == ' ' = (x,y) : getPosE (x-1) y (r-1) mapa
                   | r>0 && ((!!) ((!!) mapa y) x) == '?' = (x,y) : []
                   | otherwise = []



-- Funçao que nos dá as coordenadas explodidas numa direçao
getExpPanelE:: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanelE [] mapa = []
getExpPanelE ((x,y,r):t) mapa = (getPosE (x-1) y r mapa) ++ (getExpPanelE t mapa)


-- Funçao que diz se a coordenada de explosao é valida
getPosC :: Int->Int->Int->[String]->[(Int,Int)]
getPosC x y r mapa | r>0 && ((!!) ((!!) mapa y) x) == ' ' = (x,y) : getPosC x (y-1) (r-1) mapa
                   | r>0 && ((!!) ((!!) mapa y) x) == '?' = (x,y) : []
                   | otherwise = []



-- Funçao que nos dá as coordenadas explodidas numa direçao
getExpPanelC:: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanelC [] mapa = []
getExpPanelC ((x,y,r):t) mapa = (getPosC x (y-1) r mapa) ++ (getExpPanelC t mapa)

-- Funçao que diz se a coordenada de explosao é valida
getPosB :: Int->Int->Int->[String]->[(Int,Int)]
getPosB x y r mapa | r>0 && ((!!) ((!!) mapa y) x) == ' ' = (x,y) : getPosB x (y+1) (r-1) mapa
                   | r>0 && ((!!) ((!!) mapa y) x) == '?' = (x,y) : []
                   | otherwise = []



-- Funçao que nos dá as coordenadas explodidas numa direçao
getExpPanelB:: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanelB [] mapa = []
getExpPanelB ((x,y,r):t) mapa = (getPosB x (y+1) r mapa) ++ (getExpPanelB t mapa)

-- Funçao que nos dá as coordenadas explodidas em todas as direçoes
getExpPanel :: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanel bombs mapa = getExpPanelD bombs mapa ++ getExpPanelC bombs mapa ++ getExpPanelE bombs mapa ++ getExpPanelB bombs mapa

-- Funçao que nos da as bombas que vao explodir
getExpBomb :: [String] -> [(Int,Int,Int)]
getExpBomb mapa = map (\(a,b,c,d)->(a,b,c)) (filter (\(a,b,c,d) -> d == 0) tbombas)
                where
                ibombas = map words (filter (\a-> head a == '*') mapa )
                tbombas = map (\a-> (read ((!!) a 1) :: Int, read ((!!) a 2) :: Int, read ((!!) a 4) :: Int, read ((!!) a 5)::Int)) ibombas


replace :: Int->String->String
replace 0 (h:t) = ' ':t
replace n (s:t) = s:replace (n-1) t


replaceAt :: [(Int,Int)]->[String]->[String]
replaceAt [] mapa = mapa
replaceAt ((x,y):t) mapa = replaceAt t nmapa
    where
    (begin,(h:j))  = splitAt y mapa
    (start,(z:u)) = splitAt x h
    nmapa = begin ++ ((start ++ (' ':u)): j)
-- Retira a 1 unidade de tempo a todas as bombas
tDown :: String->String
tDown bomb = (concatMap (\a-> a++" ") nbomb) ++ time
    where 
    ibomb = words bomb
    nbomb = take 5 ibomb 
    time = show ((read (last ibomb) ::Int)-1)
-- Retira a 1 unidade de tempo a uma bomba
decTime :: [String]->[String]
decTime [] = []
decTime (h:t) | head h == '*' = tDown h : decTime t
              | otherwise = h : decTime t

-- Passa uma string a bomba
infoBomb :: String -> (String,Int,Int,Int,Int,Int)
infoBomb bomba = (a,read b::Int,read c::Int,read d::Int,read e::Int,read f::Int)
         where
         [a,b,c,d,e,f] = words bomba

-- Passa um bomba a String
showBomb::(String,Int,Int,Int,Int,Int)->String
showBomb (a,b,c,d,e,f)= a++" "++show b++" "++show c ++" "++ show d++" "++show e++" "++show f
-- Verifica quais as bombas que passam a 1
verBomAux ::[(Int,Int)]-> (String,Int,Int,Int,Int,Int) -> String
verBomAux []  bomb = showBomb bomb
verBomAux (a:b) l@(c,x,y,i,r,t)  | a == (x,y) = showBomb (c,x,y,i,r,1)
                                 | otherwise = verBomAux b l

-- Verifica quais as bombas que passam a 1
verBomb :: [String]->[(Int,Int)]->[String]
verBomb bombas coord = nbomb
                     where
                        ibomb = map infoBomb bombas
                        nbomb = map (verBomAux coord) ibomb
-- Funçao que 
remBombs :: [(Int,Int)]->[String]->[String]
remBombs coords mapa = nmapa
         where
         nmapa = upmap++nbmap++pmapa
         upmap = filter (\a-> head a =='#' || head a == '!' || head a =='!') mapa
         pmapa = filter (\a-> isDigit (head a)) mapa
         bmapa = filter (\a-> head a =='*') mapa
         nbmap = verBomb bmapa coords
-- Funçao que ve o tempo de uma bomba
seeBTime :: String->Int
seeBTime [] = 0
seeBTime bomb = f
         where
         (a,b,c,d,e,f) = infoBomb bomb
--Funçao que remove bombas explodidas
rexplosed :: [String]->[String]
rexplosed [] = []
rexplosed (h:t) | (head h == '*') && (seeBTime h == 0) = rexplosed t
                | otherwise = h : (rexplosed t)

-- Funçao que transforma mapa em um mapa explosoes
bombMap :: [String]->[String]
bombMap mapa = rexplosed rbomb
                where
                dmapa = decTime mapa
                cbomb = (getExpBomb mapa)
                pbombs = getExpPanel cbomb mapa
                nmapa = replaceAt pbombs mapa
                rbomb = remBombs pbombs nmapa


avanca :: [String] -> Int -> [String]
avanca mapa time = bombedMap
    where
    nmapa = nmap ++ npu ++ nb ++ np 
    bombedMap = bombMap nmapa
    dmap = mBlockMap mapa time
    pu = toPosPU mapa
    npu = tPUstr (rempumap dmap pu)
    p = toPosP mapa
    np = tPstr (rempmap dmap p)
    b = toPosB mapa
    nb = tBstr (rembmap dmap b)
    nmap = fromTriple dmap 0 (length $ head mapa)

main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"









