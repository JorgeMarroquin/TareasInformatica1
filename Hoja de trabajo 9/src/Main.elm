module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

type alias Modelo = String

modeloInicial : Modelo
modeloInicial = ""

type alias Mensaje = String

actualizador : Mensaje -> Modelo -> Modelo
actualizador mensaje modelo = if mensaje == "AC" then modeloInicial else if mensaje == "DEL" then borrar modelo else modelo ++ mensaje 

otro : Maybe Int -> Int
otro n = case n of
    Just a -> a
    Nothing -> 0

borrar : String -> String
borrar mod = case (String.toList (String.reverse mod)) of
    [] -> ""
    x::xs -> (String.reverse (String.fromList xs))

respaux : List Char -> List Char -> Int
respaux vlist lista = case (vlist, lista) of
    ([], []) -> 0
    ([], x::xs) -> if revisar "+" (x::xs) == True then respaux (x::[]) xs else respauxm [] (x::xs) 
    (x::xs, []) -> if revisar "+" (x::xs) == True then respaux (x::[]) xs else respauxm [] (x::(String.toList(String.reverse(String.fromList xs))))
    (x::xs, y::ys) -> if y == '+' then sumar (respaux (x::xs) []) (respaux [] ys) else respaux (x::y::xs) ys

respauxm : List Char -> List Char -> Int
respauxm vlist lista = case (vlist, lista) of
    ([], []) -> 0
    ([], x::xs) -> if revisar "x" (x::xs) == True then respauxm (x::[]) xs else otro (String.toInt (String.fromList (x::(String.toList(String.fromList xs))))) 
    (x::xs, []) -> if revisar "x" (x::xs) == True then respauxm [] (x::xs) else otro (String.toInt (String.fromList (x::(String.toList(String.reverse(String.fromList xs))))))
    (x::xs, y::ys) -> if y == 'x' then multi (respauxm (x::xs) []) (respauxm [] ys) else respauxm (x::y::xs) ys

sumar : Int -> Int -> Int
sumar n1 n2 = n1 + n2

multi : Int -> Int -> Int
multi n1 n2 = n1 * n2

revisar : String -> List Char -> Bool
revisar op exp = String.contains op (String.fromList (exp))
 
respuesta : String -> Int
respuesta modelo = respaux [] (String.toList modelo)

vista : Modelo -> Html Mensaje
vista modelo = Html.div
    []
    [ 
        Html.div [] [Html.text (modelo)],
        
        Html.div [] [
        Html.text (String.fromInt(respuesta  modelo)),
        Html.div [] [
        Html.button [style "background" "RGB(200, 200, 200)", onClick "1"] [Html.text "1"],
        Html.button [style "background" "RGB(200, 200, 200)", onClick "2"] [Html.text "2"],
        Html.button [style "background" "RGB(200, 200, 200)", onClick "3"] [Html.text "3"],
        Html.button [style "background" "RGB(82, 82, 82)", style "color" "RGB(255, 255, 255)", onClick "DEL"] [Html.text "DEL"],
        Html.div [] [
        Html.button [style "background" "RGB(200, 200, 200)", onClick "4"] [Html.text "4"],
        Html.button [style "background" "RGB(200, 200, 200)", onClick "5"] [Html.text "5"],
        Html.button [style "background" "RGB(200, 200, 200)", onClick "6"] [Html.text "6"],
        Html.button [style "background" "RGB(255, 0, 0)", style "color" "RGB(255, 255, 255)", onClick "AC"] [Html.text ".AC."],
        Html.div [] [
        Html.button [style "background" "RGB(200, 200, 200)", onClick "7"] [Html.text "7"],
        Html.button [style "background" "RGB(200, 200, 200)", onClick "8"] [Html.text "8"],
        Html.button [style "background" "RGB(200, 200, 200)", onClick "9"] [Html.text "9"],
        Html.div [] [
        Html.button [style "background" "RGB(102, 102, 102)", style "color" "RGB(255, 255, 255)", onClick "+"] [Html.text "+"],
        Html.button [style "background" "RGB(200, 200, 200)", onClick "0"] [Html.text "0"],
        Html.button [style "background" "RGB(102, 102, 102)", style "color" "RGB(255, 255, 255)", onClick "x"] [Html.text "x"]
         ] ] ] ] ]
    ] 

main = Browser.sandbox {
        init = modeloInicial,
        view = vista,
        update = actualizador
    }