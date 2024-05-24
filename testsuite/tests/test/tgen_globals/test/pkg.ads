with System;

with Global;

package Pkg is

   Supported_Global : Integer;
   Unsupported_Global : System.Address;

   Cst : constant Integer := 2;

   Multiple_Global_1, Multiple_Global_2 : Integer;

   procedure Test_Null (B : Boolean) with Global => null;

   procedure Test_Cst (B : Boolean) with Global => Cst;

   procedure Test_Supported (B : Boolean)  with Global => Supported_Global;

   procedure Test_Output (B : Boolean)
     with Global => (Output => Supported_Global);

   procedure Test_Input (B : Boolean)
     with Global =>
        (Input => (Supported_Global, Multiple_Global_1, Multiple_Global_2));

   procedure Test_In_Out (B : Boolean)
     with Global => (In_Out => Supported_Global);

end Pkg;
