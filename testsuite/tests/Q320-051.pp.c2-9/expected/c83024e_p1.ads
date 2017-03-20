with Report; use Report;
pragma Elaborate (Report);
package C83024e_P1 is
   A : Integer := Ident_Int (2);
   B : Integer := A;

   procedure Require_Body;

   generic
      X : in out Integer;
   package C83024e_Pack1 is
      C : Integer := A;
      A : Integer := Ident_Int (3);
   end C83024e_Pack1;
end C83024e_P1;
