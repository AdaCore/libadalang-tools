separate (Ca1006a)
package body Pkg is

   procedure P (I : in out Integer) is
   begin
      I := I + 3;
   end P;

begin
   I := I + 10;
end Pkg;
