--  GNATpp fix --preserve-line-breaks

package body preserve_line_breaks is
   
   procedure A is
   begin
      null;
   end A;
   

procedure P
is
   type T is array (1 .. 6) of Float;

   type R is
      record
         I : Integer;
         F : Float;
         A : T;
      end record;

begin
   null;
end P;
   
end preserve_line_breaks;
