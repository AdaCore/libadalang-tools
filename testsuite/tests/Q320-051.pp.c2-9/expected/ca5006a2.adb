-----------------------------------------------------------------------

with Report;   use Report;
with Ca5006a0; use Ca5006a0;
with Ca5006a1; use Ca5006a1;
pragma Elaborate (Ca5006a0);

package body Ca5006a2 is
   X : Integer;

   function G return Integer is
   begin
      return Ident_Int (1);
   end G;

begin
   X := F;
   if not P_E_Raised then
      Failed ("F CALLED");
   end if;
exception
   when Program_Error =>
      Comment ("PROGRAM_ERROR RAISED IN CA5006A2");
      Show_Pe_Raised;
   when others =>
      Failed ("OTHER ERROR RAISED IN CA5006A2");
end Ca5006a2;
