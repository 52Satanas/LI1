import System.Environment
import Data.List

-- Funçao que remove carateres redundantes
enc1 :: [String]->[String]
enc1 l = qenc
       where
       tam = (length l)-2
       fenc = drop 1 l
       senc = take tam fenc
       tenc = map (drop 1) senc
       qenc = map (take tam) tenc

encode :: [String] -> String
encode l = unlines $ enc++m
    where
    enc = enc1 (filter (\a->head a == '#') l)
    m = (filter (\a->head a /= '#') l)

-- Funçao que reconstroi o mapa
dec1 :: [String]->[String]
dec1 l = rep ++ nl ++ rep ++ ml 
       where
        rep = [replicate tam '#']
        nl = map (\a-> "#"++a++"#") (filter (\a->head a == ' ' || head a == '?') l)
        tam = 2+ (length $ head l )
        ml = (filter (\a->head a /= ' ' && head a /= '?') l)
decode :: String -> [String]
decode l = lines l
           
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"
