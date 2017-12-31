     --===================================================================--

with Cc54004_0;
with Cc54004_1;
with Cc54004_2;
pragma Elaborate (Cc54004_2);

package Cc54004_3 is

   package Alert_Stacks is new Cc54004_2 (Element_Type => Cc54004_0.Alert,
      Element_Ptr                                      => Cc54004_0.Alert_Ptr);

   -- All overriding versions of Handle visible at the point of instantiation.

   Alert_List : Alert_Stacks.Stack_Type;

   procedure Tc_Create_Alert_Stack;

end Cc54004_3;
