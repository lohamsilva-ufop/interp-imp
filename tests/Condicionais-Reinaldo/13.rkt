#lang interp-imp/python/z3/testz3

A = input("Digite o valor A: ")
B = input("Digite o valor B: ")
C = input("Digite o valor C: ")
D = input("Digite o valor D: ")

if (A > B) and (A > C) and (A > D) :{
   print (A)
} elif (B > A) and (B > C) and (B > D) :{
   print (B)
}
elif (C > A) and (C > B) and (C > D) :{
   print (C)
} else:{
      print (D)
}