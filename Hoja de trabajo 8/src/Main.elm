module Main exposing (..)

--Ejercicio #1

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f lista lista1 = case (lista, lista1) of 
    (_, []) -> [] 
    ([], _) -> [] 
    (x::xs, y::ys) -> f x y :: zipWith f xs ys

--Ejercicio #2

groupBy : (a -> Bool) -> List a -> (List a,List a)
groupBy f lista = (cumple f lista, ncumple f lista)

cumple f lista = case lista of 
    [] -> [] 
    (x::xs) -> if f x then cumple f xs else x :: cumple f xs

ncumple f lista = case lista of 
    [] -> [] 
    x::xs -> if f x then x :: ncumple f xs else ncumple f xs

--Ejercicio #3

bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind ju f = case ju of 
    Nothing -> Nothing 
    Just a -> f a