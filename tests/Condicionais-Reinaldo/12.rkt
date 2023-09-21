#lang interp-imp/python/z3/testz3

salario = input("Digite o salario: ")
prestacao = input("Digite o valor da prestacao: ")

proporcao = salario * 30 / 100

if prestacao < proporcao :{
   print("O emprestimo pode ser concedido")
} else:{
   print("O emprestimo não pode ser concedido")
}