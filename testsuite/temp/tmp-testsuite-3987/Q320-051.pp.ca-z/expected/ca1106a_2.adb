with Report; use Report;
with Ca1106a_2;
pragma Elaborate (Report);
package body Ca1106a_2 is
   procedure Require_Body is
   begin
      null;
   end Require_Body;
begin
   J := Tg (Ident_Int (2));
end Ca1106a_2;
