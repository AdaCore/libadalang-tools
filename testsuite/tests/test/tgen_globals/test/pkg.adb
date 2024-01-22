with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Test_Null (B : Boolean) is null;
   procedure Test_Cst (B : Boolean) is null;

   procedure Test_Supported (B : Boolean) is
   begin
      Put_Line ("Pkg.Supported_Global is " & Integer'Image (Supported_Global));
   end Test_Supported;

   procedure Test_Output (B : Boolean) is
   begin
      Put_Line ("Pkg.Supported_Global is " & Integer'Image (Supported_Global));
   end Test_Output;

   procedure Test_Input (B : Boolean) is
   begin
      Put_Line ("Pkg.Supported_Global is " & Integer'Image (Supported_Global));
   end Test_Input;

   procedure Test_In_Out (B : Boolean) is
   begin
      Put_Line ("Pkg.Supported_Global is " & Integer'Image (Supported_Global));
   end Test_In_Out;

end Pkg;
