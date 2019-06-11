module Bomber where

import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import Data.List
import Data.Char
import System.IO
import Control.Monad

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



main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"