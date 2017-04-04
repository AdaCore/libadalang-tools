-- New_Alert

 --=======================================================================--

with C393011_0;
package body C393011_3 is
   -- New_Alert

   package Definitions renames C393011_0;

   procedure Set_Serial (A : in out New_Alert_Type) is
      use type Definitions.Serial_Type;
   begin
      A.Serial_Num     := Definitions.Next;
      Definitions.Next := Definitions."+" (Definitions.Next, 1);
   end Set_Serial;

end C393011_3;
