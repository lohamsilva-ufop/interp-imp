#lang interp-imp/python/z3/testz3

numero = input("Digite um numero: ")
divisivel = numero%5

if divisivel == 0:{
   print("É divisivel por 5")
} else:{
   print("Não é divisivel por 5")
}