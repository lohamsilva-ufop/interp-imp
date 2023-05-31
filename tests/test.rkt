#lang interp-imp

print 4 + 3;
print 4 + 3 + 3 - 2 * 3;
print 4 - 3;
print 4 * 3;
print 4 / 2;
print 4 < 3;
print 3 < 4;
print 4 == 3;
print 4 == 4;
print true && true;
print !true;

int numero := 3;
print numero;

int soma := 2 + 2;
print soma;

boolean igual := true && true;
print igual;

boolean complemento := !true;
print complemento;

if 2 < 3 then
begin
print 3 + 3;
end

else

begin
print 4 + 4;
end


int valor := 0;
while valor < 5 do
begin
valor := valor + 1;
print valor;
end


for int i := 0 to 5 do
begin
i := i + 1;
print i;
end