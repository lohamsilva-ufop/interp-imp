#lang interp-imp/python/z3/testz3

numero = input("Digite um numero: ")

divisivel10 = numero%10
divisivel5 = numero%5
divisivel2 = numero%2

if divisivel10 == 0 :{
   print ("Divisivel por 10")
}
elif divisivel5 == 0 :{
   print ("Divisivel por 5")
}
elif divisivel2 == 0 :{
   print ("Divisivel por 2")
}
else: {
   print ("Não é divisivel por 10, 5 ou 2")
}