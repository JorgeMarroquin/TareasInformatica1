module Main exposing (..)


--EJERCICIO #1
suma : Int -> Int -> Int
suma n1 n2 = n1 + n2

multiplicacion : Int -> Int -> Int
multiplicacion n1 n2 = n1 * n2

--EJERCICIO #2

type Expresion = Val Int | Suma Expresion Expresion | Mult Expresion Expresion

reducir (sum, multi) exp = case exp of
    Suma exp1 exp2 -> sum (reducir (sum, multi) exp1) (reducir (suma, multi) exp2)
    Mult exp1 exp2 -> multi (reducir (sum, multi) exp1) (reducir (sum, multi) exp2)
    Val i -> i
