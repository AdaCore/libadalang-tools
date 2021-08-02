with Foo; use Foo;

package Bar is

   procedure Integer_Swap is new Swap (Integer);
   procedure I_Swap (A, B : in out Integer) renames Integer_Swap;

   procedure Float_Swap (A, B : Float);

   package Nested is
      procedure Integer_Copy_To is new Copy_To (Integer);
   end Nested;

end Bar;
