#lang interp-imp/z3/testz3
int x := 1;

if x < 10 then
begin
   if x < 9 then
       begin
         if x < 8  then
           begin
             print 3 + 3;
           end
        else
          begin
          print 4 + 4;
       end 
   end
   else
       begin
          print 4 + 4;
   end
end

else
begin
   if x < 3 then
       begin
           print 3 + 3;
           if x < 4 then
             begin
               print 3 + 3;
             end
         else
            begin
               print 4 + 4;
            end 
       end
   else
       begin
          print 4 + 4;
          int sum:= 5 + 5;
       end
end
