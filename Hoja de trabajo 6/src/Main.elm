module Main exposing (..)

-- EJERCICIO #1
type Natural = Suc Natural | Cero

enteroanatural : Int -> Natural
enteroanatural i = if i == 0 then Cero else Suc (enteroanatural (i - 1))

sumar : Natural -> Natural -> Natural
sumar n1 n2 = case (n1, n2) of 
    (Cero, n2_) -> n2_ 
    (n1_, Cero) -> n1_ 
    (Suc n1_, n2_) -> Suc (sumar n1_ n2_)

resta : Natural -> Natural -> Natural
resta n1 n2 = case (n1, n2) of
    (Cero, n2_) -> Cero
    (n1_, Cero) -> n1_
    (Suc n1_, Suc n2_) -> resta n1_ n2_

multiplicacion : Natural -> Natural -> Natural
multiplicacion n1 n2 =  case (n1, n2) of
    (Cero, n2_) -> Cero
    (n1_, Cero) -> Cero
    (Suc n1_, n2_) -> sumar n2_ (multiplicacion n1_ n2_)

division : Natural -> Natural -> (Natural, Natural)
division n1 n2 = if multiplicacion (div n1 n2 Cero) n2 == n1 then (div n1 n2 Cero, Cero) else
    (resta (div n1 n2 Cero) (Suc Cero), resta n1 (multiplicacion (resta (div n1 n2 Cero) (Suc Cero)) n2))

div : Natural -> Natural -> Natural -> Natural
div n1 n2 n3 = case (n1, n2, n3) of
    (Cero, n2_, n3_) -> n3_
    (n1_, Cero, _) -> Cero
    (n1_, n2_, n3_) ->  div (resta n1_ n2_) n2_ (sumar n3_ (Suc Cero))

--EJERCICIO #2
--type Expresion = Val Int | Suma Expresion Expresion | Mult Expresion Expresion

--EJERCICIO #3

type GExpresion a = Val a | Suma (GExpresion a) (GExpresion a) | Mult (GExpresion a) (GExpresion a)

type alias Expresion = GExpresion Int

type Estado = Final Int | Parcial (List Char)

