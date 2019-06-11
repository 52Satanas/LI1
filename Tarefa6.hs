module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe

--import Tarefa6_li1g101 (bot)

-- Funçao que cria um mapa de triplos (Char,Int,Int)
toTriple :: [String]->Int->Int->[(Char,Int,Int)]
toTriple [] _ _= []
toTriple (h:t) l size = res ++ toTriple t (l+1) size
            where
                col = enumFromThenTo 0 1 size
                lin = replicate size l
                res = zip3 h col lin
-- Funçao que cria um mapa de coordenadas atraves de um mapa de triplos (Char,Int,Int)
totuple::[(Char,Int,Int)]->[(Int,Int)]
totuple l = map (\(a,b,c)->(b,c)) (filter (\(a,b,c)->a/='#') l)

-- Funçao que cria as coordenadas de um jogador
createpos :: [Int] ->(Int,Int)
createpos [] = (-1,-1)
createpos [a,b] = (a,b)

-- Funçao que retorna o movimento do jogador
maybeMove :: [(Char,Int,Int)] -> (Int,Int)->Char
maybeMove [] p = 't'
maybeMove mapa (x,y) | elem (x,y+1) nmapa = 'U'
                     | elem (x,y-1) nmapa = 'D'
                     | elem (x-1,y) nmapa = 'E'
                     | elem (x+1,y) nmapa = 'R' 
          where
          nmapa = totuple mapa

bot :: [String] -> Int -> Int -> Maybe Char
bot mapa ticks player | car == 't' = Nothing
                      | otherwise = Just car
    where
        cmapa = toTriple mapa 0 (length mapa)
        infop =  (filter (\a-> take 1 a == show player) mapa)
        infop2 = words $ head infop
        posp = createpos (map (\a -> read a ::Int) (take 2 (tail infop2)))
        car = maybeMove cmapa posp

main :: IO ()
main = do
    a <- getArgs
    let player = readMaybe (a !! 0)
    let ticks = readMaybe (a !! 1)
    w <- getContents
    if isJust player && isJust ticks
        then putStr $ show $ bot (lines w) (fromJust player) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"
