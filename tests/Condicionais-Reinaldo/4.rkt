#lang interp-imp/python/z3/testz3
numero = input("Insira um valor")

if numero > 0 :{
    print ("POSITIVO")
} else:{
    if numero < 0 :{
    print ("NEGATIVO")
   } else:{
     print ("NULO")
   }
}

