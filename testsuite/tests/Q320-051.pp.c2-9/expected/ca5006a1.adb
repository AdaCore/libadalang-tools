-----------------------------------------------------------------------

with Report;   use Report;
with Ca5006a0; use Ca5006a0;
with Ca5006a2; use Ca5006a2;
pragma Elaborate (Ca5006a0);

package body Ca5006a1 is
   X : Integer;

   function F return Integer is
   begin
      return Ident_Int (0);
   end F;

begin
   X := G;
   if not P_E_Raised then
      Failed ("G CALLED");
   end if;
exception
   when Program_Error =>
      Comment ("PROGRAM_ERROR RAISED IN CA5006A1");
      Show_Pe_Raised;
   when others =>
      Failed ("OTHER ERROR RAISED IN CA5006A1");
end Ca5006a1;
