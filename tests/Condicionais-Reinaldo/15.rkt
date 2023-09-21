#lang interp-imp/python/plugin

A = input("Digite o valor A: ")
B = input("Digite o valor B: ")
C = input("Digite o valor C: ")

if (A > B) and (A > C) and (B > C) :{
   print (A)
   print (B)
   print (C)
} elif (A > B) and (A > C) and (C > B) :{
   print (A)
   print (C)
   print (B)
}
elif (B > A) and (B > C) and (A > C) :{
   print (B)
   print (A)
   print (C)
}
elif (B > A) and (B > C) and (C > A) :{
   print (B)
   print (C)
   print (A)
}
elif (C > A) and (C > B) and (A > B) :{
   print (C)
   print (A)
   print (B)
} else:{
      print (C)
      print (B)
      print (A)
   }