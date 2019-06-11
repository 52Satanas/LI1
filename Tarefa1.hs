module Tarefa1 where

import System.Environment
import Text.Read
import Data.Maybe
import System.Random 
import Data.List

-- Calcula o tamanho do take a ser feito
sS :: Int->Int
sS dim = dim*dim - pedra - centro - vazio
            where
              pedra = dim * 2 + (dim-2)*2
              centro = ((div (dim -2) 2)) * ((div (dim -2) 2)) 
              vazio = 4*3
-- Funçao que transforma uma lista de inteiros em carateres 
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


main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"-}