with Ada.Text_IO; use Ada.Text_IO;

package body A is  -- Rename A to: B, Z

   procedure E is
   begin
      Put_Line ("Procedure E from package A");
   end E;
   
   task body F is
   begin
      null;
   end F;

   task body G is
   begin
      null;
   end G;

   package body I is

      procedure Print_I (I_Spec : I_Type) is
      begin
         null;
      end Print_I;

   end I;
   
   package body J is

      procedure Print_J (J_Spec : J_Type) is
      begin
         null;
      end Print_J;

   end J;
   
end A;
