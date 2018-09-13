module Main exposing (esPrimo, fibonacci, primos, nPrimos)

esPrimo : Int -> Bool
esPrimo x = contador 2 x
contador x y =     
    if y == 2 then True else 
    if modBy x y == 0 then False else  
    if x == y - 1 then True else contador (x + 1) y

fibonacci : Int -> Int
fibonacci x = if x == 0 then 0 else if x == 1 then 1 else if x > 1 then fibonacci (x - 1) + fibonacci (x - 2) else 0

primos : Int -> List Int
primos x = if x < 2 then [] else if esPrimo x == False then primos (x - 1) else x :: primos (x - 1)


nPrimos : Int -> List Int
nPrimos x = contador3 (x, 2)
contador3 (x, y) = if x == 0 then [] else if esPrimo y == False then contador3 (x, y + 1) else y :: contador3 (x - 1, y + 1)