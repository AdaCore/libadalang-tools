with Report; use Report;
package body Ca5003a0 is

   function Show_Elab (Unit : Character) return Integer is
   begin
      Order (Index) := Unit;
      Index         := Index + 1;
      return Index - 1;
   end Show_Elab;

end Ca5003a0;
