with Report; use Report;
pragma Elaborate_All (Report);
with Ca15003a.Pure_Instance;
with Ca15003a.Pure_Preelaborate_Instance; use Ca15003a;
package body Ca15003a.Empty_Elaborate_Body is
begin
   if Two /= Big_Positive'Base (Ident_Int (0)) then
      Failed ("Two should be zero now");
   end if;
   if Tres /= Big_Positive'Base (Ident_Int (0)) then
      Failed ("Tres should be zero now");
   end if;
   if Two /= Tres then
      Failed ("Tres should be zero now");
   end if;
   Two  := Pure_Instance.F (Three'Access);
   Tres := Pure_Preelaborate_Instance.F (Three'Access);
   if Two /= Big_Positive (Ident_Int (2)) then
      Failed ("Two should be 2 now");
   end if;
   if Tres /= Big_Positive (Ident_Int (3)) then
      Failed ("Tres should be 3 now");
   end if;
end Ca15003a.Empty_Elaborate_Body;
