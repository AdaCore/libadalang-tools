with Report; use Report;
pragma Elaborate (Report);
package C83024e_P3 is
   A : Integer := Ident_Int (2);
   B : Integer := A;

   procedure Require_Body;

   generic
      X : in out Integer;
   package C83024e_Pack3 is
   end C83024e_Pack3;
end C83024e_P3;
