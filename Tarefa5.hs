module Main where
import Codec.BMP
import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import System.Random 
import System.Environment
import Data.Maybe
import System.Random 
import Data.List
import Data.Char
import Text.Read hiding (Char)


type Size = Int
type CFloor = [(Char,Int,Int)]
type Floor =  [String]
-- | Uma representação do estado do jogo.
type Estado = ([String],[String],[Picture])

-- | O estado inicial do jogo.
estadoInicial :: Int->[Picture]->Estado
estadoInicial b p= (nmapa++["0 1 1"],ppu ,p)
                 where
                 nmapa = mapa 11 b
                 ppu = filter (\a-> head a /='#') nmapa




-- | Função que altera o estado do jogo quando acontece um evento.
reageEvento :: Event ->Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (mapa,pu,pic) = (nmapa,npu,pic)
               where
               nmapa = move mapa 0 'D'
               npu = filter (\a->head a /= '#') nmapa
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (mapa,pu,pic) = (nmapa,npu,pic)
               where
               nmapa = move mapa 0 'U'
               npu = filter (\a->head a /= '#') nmapa
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (mapa,pu,pic) = (nmapa,npu,pic)
               where
               nmapa = move mapa 0 'L'
               npu = filter (\a->head a /= '#') nmapa
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (mapa,pu,pic) = (nmapa,npu,pic)
               where
               nmapa = move mapa 0 'R'
               npu = filter (\a->head a /= '#') nmapa
reageEvento (EventKey (Char 'b') Down _ _) (mapa,pu,pic) = (nmapa,npu,pic)
               where
               nmapa = move mapa 0 'B'
               npu = filter (\a->head a /= '#') nmapa
reageEvento _ mapa = mapa

-- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo f l@(mapa,pu,pic) = l
                           where
                           nmapa = avanca mapa ((floor f)+1000)
                           npu = filter (\a->head a /= '#') nmapa 

-- | Frame rate
fr :: Int
fr = 50

-- | Display mode
dm :: Display
dm = InWindow "Novo Jogo" (800, 600) (0, 0)
    
-- | Função principal que invoca o jogo.
desenhaEstado :: Estado->Picture
desenhaEstado estado = Pictures (desenhaM estado 0 0)



-- Funçao que desenha o estado do jogo
desenhaM  :: Estado->Int->Int -> [Picture]
desenhaM ([],[],p) x y = []
desenhaM ((h:t),[],p) x y = desenhaF h x y p ++ desenhaM (t,[],p) x (y+1)
desenhaM (l,b,p) x y = desenhaP b p ++ desenhaM (l,[],p) x y


--Funçao que desenha bombas e powerups
desenhaP :: [String] ->[Picture]->[Picture]
desenhaP [] pic = []
desenhaP (h:t) pic| head h == '+' = (translate i j ((!!) pic 0)) :desenhaP t pic
                      | head h == '!' = (translate i j ((!!) pic 1)) :desenhaP t pic
                      | head h == '*' = (translate i j ((!!) pic 2)) :desenhaP t pic
                      | otherwise = desenhaP t pic
                      where
                      info = words h
                      infox =read ((!!) info 1) ::Int
                      infoy =read ((!!) info 2) ::Int
                      dx = div (-infox +500) 2
                      dy = div (-infoy +300) 2
                      i = intToFloat (dx + 44 * infox)
                      j = intToFloat (dy + 44 * infoy)                      

--Funçao que desenha o mapa 
desenhaF :: String->Int->Int -> [Picture]->[Picture]
desenhaF [] x y pic= []
desenhaF l@(h:t) x y pic | h == '?' = (translate i j ((!!) pic 5)) :desenhaF t (x+1) y pic
                      | h == '#' = (translate i j ((!!) pic 6)) :desenhaF t (x+1) y pic
                      | h == ' ' = (translate i j ((!!) pic 4))  :desenhaF t (x+1) y pic
                      | h == '0' = (translate ip jp ((!!) pic 3))  :desenhaF t x y pic
                      | otherwise = desenhaF t x y pic
                   where
                   player = words l 
                   infox =read ((!!) player 1) ::Int
                   infoy =read ((!!) player 2) ::Int
                   dxp = div (-(infox+500)) 2
                   dyp = div (-(infoy+300)) 2
                   ip = intToFloat (dxp + 44 * infox)
                   jp = intToFloat (dyp + 44 * infoy)
                   dx = div (-(x+500)) 2
                   dy = div (-(y+300)) 2
                   i = intToFloat (dx + 44 * x)
                   j = intToFloat (dy + 44 * y)




intToFloat::Int->Float
intToFloat a = fromIntegral a

main :: IO ()
main = do a <- getArgs 
          pu1 <- loadBMP "BMP/bomba.bmp" 
          pu2 <- loadBMP "BMP/range.bmp"
          bomba <- loadBMP "BMP/bomba.bmp"
          num1 <- loadBMP "BMP/num1.bmp"
          panel <- loadBMP "BMP/painel.bmp"
          tijolo <- loadBMP "BMP/tijolo.bmp"
          pedra <- loadBMP "BMP/pedra.bmp"
          let l = readMaybe (a !! 0)
          let p = [pu1,pu2,bomba,num1,panel,tijolo,pedra]
          play dm 
               (greyN 0.5) 
               fr 
               (estadoInicial (fromJust l) p) 
               desenhaEstado 
               reageEvento 
               (reageTempo)
          









sS :: Int->Int
sS dim = dim*dim - pedra - centro - vazio
            where
              pedra = dim * 2 + (dim-2)*2
              centro = ((div (dim -2) 2)) * ((div (dim -2) 2)) 
              vazio = 4*3

confC::[Int]->String
confC [] = []
confC (a:t)   |0<=a && a<=1   = '+' : confC t
              |2<=a && a<=3   = '!' : confC t
              |4<=a && a<=39  = '?' : confC t
              |40<=a && a<=99 = ' ' : confC t


-- Gera as celulas do mapa
gC::Int->Int->[Int]
gC dim seed = take size $ randomRs (0,99) (mkStdGen seed)
       where
       size = sS dim 
-- Gera a uma string de n '#'
gFL::Int->String
gFL dim = replicate dim '#'
-- Gera a segunda e penultima linha do mapa
gSL::String->String
gSL cel = "#  "++cel++"  #"


--
intercale :: String->String->String
intercale [] l = l
intercale l [] = l
intercale (a:b) (x:t) = a:x:(intercale b t)

-- Gera a terceira e dim-2 linha do mapa
gTL::String->Int->String
gTL cel dim = "# "++intercale blocos cel ++" #"
            where
            blocos = replicate (div dim 2) '#'


-- Gera uma linha do mapa
gL::String->String
gL cel = "#"++cel++"#"

-- Gera uma linha par do mapa
gLP::String->String
gLP cel = "#"++ intersperse '#' cel ++"#"

-- Gera uma linha impar do mapa
gLI :: String->Int->String
gLI cel dim = "#"++intercale cel blocos ++"#"
            where
              blocos = replicate (div dim 2) '#'
-- Gera todas as linhas do mapa á exceçao da 1º, 2º , n-1º e nº linhas  
gLC :: String->Int->Int->[String]
gLC [] n c = []
gLC celulas linha dim | linha == 1   = gTL init dim : gLC nextcel (linha+1) dim
                      | linha == dim-2 = gTL init dim : []
                      | even linha   = gL lPar : gLC nCelP (linha+1) dim
                      | odd linha    = gLI lImpar dim : gLC nCelI (linha+1) dim
                      where
                        init    = take ((div dim 2)-1) celulas
                        lPar    = take dim celulas
                        nCelP   = drop dim celulas
                        lImpar  = take ((div dim 2)+1) celulas
                        nCelI   = drop ((div dim 2)+1) celulas
                        nextcel = drop ((div dim 2)-1) celulas

-- Coloca no mapa aquilo que foi pela seed
bMap:: Int -> String->[String]
bMap dim [] = []
bMap dim celulas = [(gFL dim), (gSL cel2)] ++ (gLC mcel 1 (dim-2) ) ++ [(gSL celn2),(gFL dim)]
                 where
                 cel2  = take (dim-6) celulas
                 celn2 = drop ((length celulas)-(dim-6)) celulas
                 mcel  = take ((length celulas)-(dim-6)*2)  $ drop (dim-6) celulas

crit a b | head a == '+' = LT
         | head a == '!' = GT
         | otherwise = EQ
-- Calcula as coordenadas de um power up
fxCoords :: String->Int->Int->[String]
fxCoords [] x y = []
fxCoords (h:t) x y | h == '!' || h == '+' = (h: " "++show x++" "++show y) : fxCoords t (x+1) y
                   | otherwise = fxCoords t (x+1) y
-- Calcula as coordenadas de um power up
fCoords :: [String]->Int->[String]
fCoords [] y = []
fCoords (h:t) y = sortBy crit (fxCoords h 0 y ++ fCoords t (y+1))

-- Funçao que colaca ' ' no lugar de '!' ou '+'
repl :: String -> String
repl [] = []
repl (h:t) | h == '+' || h == '!' = '?' : repl t
           | otherwise = h : repl t

mapa :: Int -> Int -> [String]
mapa 5 seed = ["#####","#   #","# # #","#   #","#####"]
mapa dim seed = (map repl newm) ++ pu
  where
  celulasI = gC dim seed
  celulasS = confC celulasI
  newm     = bMap dim celulasS
  pu       = fCoords newm 0



-- Verifica se o movimento é valido
verMove :: [String]->(String,Int,Int,String)-> Bool
verMove mapa (a,x,y,c) | (!!) ((!!) mapa y) x == ' ' = True
                       | otherwise = False
-- Filtra a info do jogador a mover
gP :: [String]->Int->(String,Int,Int,String)
gP mapa id | length player == 3 = (head player,read ((!!) player 1) ::Int,read ((!!) player 2) ::Int, "")
           | otherwise = (head player,read ((!!) player 1) ::Int,read ((!!) player 2) ::Int, (!!) player 3)
           where
           player = words $ head $filter (\a-> head a == intToDigit id) mapa

-- Filtra os power ups do mapa para triplo
gPU :: [String]->[(String,Int,Int)]
gPU mapa = map (\[a,b,c]->(a, read b ::Int,read c ::Int)) (map (\a-> words a) (filter (\a-> head a == '+' || head a == '!') mapa))

-- Transforma um triplo de power up em string
puTS ::[(String,Int,Int)]->[String]
puTS pu = map (\(a,b,c)->a++" "++show b ++ " "++show c) pu

-- Transforma um tuplo de jogador em string
pTS :: (String,Int,Int,String)-> String
pTS (a,b,c,d) = a ++" "++ show b ++" "++ show c ++ " "++ d

-- Retira os power ups do mapa
verMPU:: (String,Int,Int,String)-> [(String,Int,Int)]->[(String,Int,Int)]
verMPU p [] = []
verMPU p@(a,b,c,d) (h@(z,x,y):t) | b==x && c ==y = verMPU p t
                                 | otherwise = h: (verMPU p t)


-- Move o jogador para a posiçao indicada pelo carater
addMove :: (String,Int,Int,String)->Char ->(String,Int,Int,String)
addMove (a,b,c,d) 'L' = (a,b-1,c,d) 
addMove (a,b,c,d) 'R' = (a,b+1,c,d) 
addMove (a,b,c,d) 'D' = (a,b,c+1,d)  
addMove (a,b,c,d) 'U' = (a,b,c-1,d)
addMove (a,b,c,d)  _  = (a,b,c,d)
-- Adiciona os power ups ao jogador
addPU :: (String,Int,Int,String)->[(String,Int,Int)]->(String,Int,Int,String)
addPU p [] = p
addPU p@(a,b,c,d) (h@(z,x,y):t) | b==x && c ==y = (a,b,c,d++z)
                                | otherwise = (addPU p t)


-- Move o jogador
movePos :: [String]->Int->Char->[String]
movePos mapa id move | verMove mapa player = floor ++ (puTS (verMPU player pu))  ++ bombs ++ (insert (pTS player) jog)
                     | otherwise = mapa
                     where
                     player = addPU (addMove (gP mapa id) move) pu
                     pu = gPU mapa
                     floor = filter (\a->head a =='#') mapa
                     bombs = filter (\a->head a =='*') mapa
                     jog = filter (\a->head a /= intToDigit id && (isDigit $ head a)) mapa

-- Calcula o range de um player
gRange :: (String,Int,Int,String)->Int
gRange (a,b,c,d) = length d
-- Calcula o (x,y) de um jogador
gPosP :: (String,Int,Int,String)->(Int,Int)
gPosP (a,b,c,x) = (b,c)
-- Coloca uma bomba no mapa
colocaBomba :: [String]->Int->[String]
colocaBomba mapa id = floor ++ pu ++ bombs ++ player
                where
                    range = gRange (gP mapa id)
                    pos   = gPosP (gP mapa id)
                    floor = filter (\a->head a =='#') mapa
                    player = filter (\a->isDigit $ head a) mapa
                    pu = filter (\a->head a =='+' || head a == '!') mapa
                    bombs = insert newBomb (filter (\a->head a =='*') mapa)
                    newBomb =  "*" ++ " " ++ show (fst pos) ++ " " ++ show (snd pos) ++ " " ++ show id ++ " " ++ show range ++" " ++"10"

move :: [String] -> Int -> Char -> [String]
move mapa id move | move == 'B' = colocaBomba mapa id
                  | otherwise = movePos mapa id move

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
crit2 (a,b,c) (d,e,f)  | b == e && c < f = LT
                      | b < e = LT
                      | otherwise = GT

-- Funçao que destroi o mapa
mBlockMap :: [String]->Int->CFloor
mBlockMap mapa time | blocos>0 = bMap 
                    | otherwise = cmapa
    where
        bMap = (sortBy crit2 (blockMap cmapa 1 1 blocos tam tam dir))
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

getPosD :: Int->Int->Int->[String]->[(Int,Int)]
getPosD x y r mapa | r>0 && ((!!) ((!!) mapa y) x) == ' ' = (x,y) : getPosD (x+1) y (r-1) mapa
                   | r>0 && ((!!) ((!!) mapa y) x) == '?' = (x,y) : []
                   | otherwise = []


getExpPanelD:: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanelD [] mapa = []
getExpPanelD ((x,y,r):t) mapa = (getPosD (x+1) y r mapa) ++ (getExpPanelD t mapa)



getPosE :: Int->Int->Int->[String]->[(Int,Int)]
getPosE x y r mapa | r>0 && ((!!) ((!!) mapa y) x) == ' ' = (x,y) : getPosE (x-1) y (r-1) mapa
                   | r>0 && ((!!) ((!!) mapa y) x) == '?' = (x,y) : []
                   | otherwise = []




getExpPanelE:: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanelE [] mapa = []
getExpPanelE ((x,y,r):t) mapa = (getPosE (x-1) y r mapa) ++ (getExpPanelE t mapa)



getPosC :: Int->Int->Int->[String]->[(Int,Int)]
getPosC x y r mapa | r>0 && ((!!) ((!!) mapa y) x) == ' ' = (x,y) : getPosC x (y-1) (r-1) mapa
                   | r>0 && ((!!) ((!!) mapa y) x) == '?' = (x,y) : []
                   | otherwise = []




getExpPanelC:: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanelC [] mapa = []
getExpPanelC ((x,y,r):t) mapa = (getPosC x (y-1) r mapa) ++ (getExpPanelC t mapa)


getPosB :: Int->Int->Int->[String]->[(Int,Int)]
getPosB x y r mapa | r>0 && ((!!) ((!!) mapa y) x) == ' ' = (x,y) : getPosB x (y+1) (r-1) mapa
                   | r>0 && ((!!) ((!!) mapa y) x) == '?' = (x,y) : []
                   | otherwise = []




getExpPanelB:: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanelB [] mapa = []
getExpPanelB ((x,y,r):t) mapa = (getPosB x (y+1) r mapa) ++ (getExpPanelB t mapa)

getExpPanel :: [(Int,Int,Int)]->[String]->[(Int,Int)]
getExpPanel bombs mapa = getExpPanelD bombs mapa ++ getExpPanelC bombs mapa ++ getExpPanelE bombs mapa ++ getExpPanelB bombs mapa


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

tDown :: String->String
tDown bomb = (concatMap (\a-> a++" ") nbomb) ++ time
    where 
    ibomb = words bomb
    nbomb = take 5 ibomb 
    time = show ((read (last ibomb) ::Int)-1)

decTime :: [String]->[String]
decTime [] = []
decTime (h:t) | head h == '*' = tDown h : decTime t
              | otherwise = h : decTime t


infoBomb :: String -> (String,Int,Int,Int,Int,Int)
infoBomb bomba = (a,read b::Int,read c::Int,read d::Int,read e::Int,read f::Int)
         where
         [a,b,c,d,e,f] = words bomba


showBomb::(String,Int,Int,Int,Int,Int)->String
showBomb (a,b,c,d,e,f)= a++" "++show b++" "++show c ++" "++ show d++" "++show e++" "++show f

verBomAux ::[(Int,Int)]-> (String,Int,Int,Int,Int,Int) -> String
verBomAux []  bomb = showBomb bomb
verBomAux (a:b) l@(c,x,y,i,r,t)  | a == (x,y) = showBomb (c,x,y,i,r,1)
                                 | otherwise = verBomAux b l


verBomb :: [String]->[(Int,Int)]->[String]
verBomb bombas coord = nbomb
                     where
                        ibomb = map infoBomb bombas
                        nbomb = map (verBomAux coord) ibomb

remBombs :: [(Int,Int)]->[String]->[String]
remBombs coords mapa = nmapa
         where
         nmapa = upmap++nbmap++pmapa
         upmap = filter (\a-> head a =='#' || head a == '!' || head a =='!') mapa
         pmapa = filter (\a-> isDigit (head a)) mapa
         bmapa = filter (\a-> head a =='*') mapa
         nbmap = verBomb bmapa coords

seeBTime :: String->Int
seeBTime [] = 0
seeBTime bomb = f
         where
         (a,b,c,d,e,f) = infoBomb bomb

rexplosed :: [String]->[String]
rexplosed [] = []
rexplosed (h:t) | (head h == '*') && (seeBTime h == 0) = rexplosed t
                | otherwise = h : (rexplosed t)


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

