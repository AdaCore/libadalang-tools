with Report; use Report;
with C83024e_Gen_Fun;
pragma Elaborate (Report, C83024e_Gen_Fun);
package C83024e_P4 is
   Obj : Integer := Ident_Int (1);
   Flo : Float   := 6.25;

   procedure Require_Body;

   function F is new C83024e_Gen_Fun (Integer, Obj);
   function F is new C83024e_Gen_Fun (Float, Flo);

   generic
      X : in out Integer;
      F : in Float;
   package C83024e_Pack4 is
   end C83024e_Pack4;
end C83024e_P4;
