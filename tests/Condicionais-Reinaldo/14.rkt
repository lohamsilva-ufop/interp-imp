#lang interp-imp/python/z3/testz3
A = input("Digite o valor A: ")
B = input("Digite o valor B: ")
C = input("Digite o valor C: ")


if (A > B) and (A > C) :{
   print("O maior valor e:")
   print (A)
} else:{
   if (B > A) and (B > C) :{
   print("O maior valor e:")
   print (B)
   } else :{
      print("O maior valor e:")
      print (C)
   }
}