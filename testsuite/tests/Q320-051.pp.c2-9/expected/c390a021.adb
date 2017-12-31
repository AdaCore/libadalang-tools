     --==================================================================--

with F390a00;  -- Basic alert abstraction.
use F390a00;
package body C390a021 is

   use C390a020;  -- Extended alert abstraction.

   procedure Assign_Officer (Ma : in out Medium_Alert_Type;
      To                        : in     Person_Enum)
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

   function Initial_Values_Okay (Ma : in Medium_Alert_Type) return Boolean is
   begin
      return
        (Ma =
         (Arrival_Time   => Default_Time,      -- Check "=" operator
          Display_On     => Null_Device,       -- availability.
          Level          => 0,                 -- Aggregate with
          Action_Officer => Nobody));          -- named associations.
   end Initial_Values_Okay;

   function Bad_Final_Values (Ma : in Medium_Alert_Type) return Boolean is
   begin
      return (Ma /= (Alert_Time,
          Console,                -- Check "/=" operator
          2, Duty_Officer));         -- availability.
   end Bad_Final_Values;                                 -- Aggregate with
   -- positional assoc.

end C390a021;
