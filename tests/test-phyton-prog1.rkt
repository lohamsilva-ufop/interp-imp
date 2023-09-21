#lang interp-imp/python/plugin
a = 3
b = 4
soma = a + b

print('A soma de a + b é igual a soma')
print (soma)

if soma > 8: {
   print('A soma é maior que 8') 
}

if soma < 8: {
   print('A soma é menor que 8') 
}
else :{
   print('A soma é igual a 0') 
}

valor = input('Digite um numero: ')