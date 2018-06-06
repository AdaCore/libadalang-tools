     --==================================================================--

package body C390a010 is

   use F390a00;  -- Alert system abstraction.

   function Level_Of (La : in Low_Alert_Type) return Integer is
   begin
      return (La.Level + 1);
   end Level_Of;

   procedure Handle (La : in out Low_Alert_Type) is
   begin
      Handle (Alert_Type (La));          -- Call parent's op (type conversion).
      La.Level      := Level_Of (La);         -- Call newly declared operation.
      La.Display_On := Teletype;
      Display (La);                      -- Call inherited operation.
   end Handle;

   procedure Assign_Officer
     (Ma : in out Medium_Alert_Type; To : in Person_Enum)
   is
   begin
      Ma.Action_Officer := To;
   end Assign_Officer;

   procedure Handle (Ma : in out Medium_Alert_Type) is
   begin
      Handle (Low_Alert_Type (Ma));      -- Call parent's op (type conversion).
      Ma.Level := Level_Of (Ma);         -- Call inherited operation.
      Assign_Officer (Ma, Duty_Officer); -- Call newly declared operation.
      Ma.Display_On := Console;
      Display (Ma);                      -- Call twice-inherited operation.
   end Handle;

end C390a010;
