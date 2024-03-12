with Ada.Numerics; use Ada.Numerics;

package Pkg is
   type Precision_6 is digits 6;
   subtype My_Float is Precision_6 range -Pi .. Pi;
   procedure Foo (P1 : My_Float);
end Pkg;
