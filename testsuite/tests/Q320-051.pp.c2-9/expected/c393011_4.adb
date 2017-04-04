package body C393011_4 is

   procedure Display (Fa : in Final_Alert_Type) is
   begin
      Definitions.Display_Device := Fa.Display_Dev;
   end Display;

   procedure Handle (Fa : in out Final_Alert_Type) is
   begin
      Set_Status (Fa, Definitions.Handled);
      Set_Serial (Fa);
      Display (Fa);
   end Handle;
end C393011_4;
