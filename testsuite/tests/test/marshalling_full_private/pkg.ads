with Ada.Strings.Text_Buffers.Utils; use Ada.Strings.Text_Buffers.Utils;

package Pkg is
   type P_Type is private;
   procedure Foo (P : P_Type);
   type A (<>) is private;
   procedure Bar (X : A);
   procedure Baz (S : UTF_8_Lines);
private
   type I_Type is range 0 .. 1;
   type P_Type is array (I_Type) of Boolean;

      type A (Discr : I_Type) is record
      case Discr is
         when 1 =>
            F1 : Integer;
            F2 : Integer;
         when others =>
            F3 : Integer;
      end case;
   end record;

end Pkg;
