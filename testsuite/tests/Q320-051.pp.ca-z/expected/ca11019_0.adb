-- parent.child

     --=================================================================--

-- parent body depends on private generic child
with Ca11019_0.Ca11019_1;     -- Private generic child.

pragma Elaborate (Ca11019_0.Ca11019_1);
package body Ca11019_0 is

   -- instantiate the generic child with data types needed by the package
   -- interface services
   package Data_1_Ops is new Ca11019_1 (Data_Type => Data_1);

   package Data_2_Ops is new Ca11019_1 (Data_Type => Data_2);

   package Data_3_Ops is new Ca11019_1 (Data_Type => Data_3);

   ---------------------------------------------------------

   procedure Add_1 (Data : Data_1; To : in out Data_Collection) is
   begin
      -- maybe do other stuff here
      Data_1_Ops.Add (Data, To);
      -- and here
   end Add_1;

   ---------------------------------------------------------

   function Statistical_Op_1 (Data : Data_Collection) return Data_1 is
   begin
      -- maybe use generic operation(s) in some complicated ways (but
      -- simplified out, for the sake of testing)
      return Data_1_Ops.Op (Data);
   end Statistical_Op_1;

   ---------------------------------------------------------

   procedure Add_2 (Data : Data_2; To : in out Data_Collection) is
   begin
      Data_2_Ops.Add (Data, To);
   end Add_2;

   ---------------------------------------------------------

   function Statistical_Op_2 (Data : Data_Collection) return Data_2 is
   begin
      return Data_2_Ops.Op (Data);
   end Statistical_Op_2;

   ---------------------------------------------------------

   procedure Add_3 (Data : Data_3; To : in out Data_Collection) is
   begin
      Data_3_Ops.Add (Data, To);
   end Add_3;

   ---------------------------------------------------------

   function Statistical_Op_3 (Data : Data_Collection) return Data_3 is
   begin
      return Data_3_Ops.Op (Data);
   end Statistical_Op_3;

end Ca11019_0;
