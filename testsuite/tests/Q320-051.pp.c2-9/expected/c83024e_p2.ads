with Report; use Report;
pragma Elaborate (Report);
package C83024e_P2 is
   A   : Integer := Ident_Int (2);
   B   : Integer := A;
   Obj : Integer := Ident_Int (3);

   procedure Require_Body;

   generic
      X : in Integer := A;
      A : in out Integer;
   package C83024e_Pack2 is
      C : Integer := A;
   end C83024e_Pack2;
end C83024e_P2;
