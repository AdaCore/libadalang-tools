     --==================================================================--

with C3900050;            -- Basic alert abstraction.
with Ada.Calendar;
pragma Elaborate (Ada.Calendar);

package body C3900052 is  -- Further extended alert system abstraction.

   use C3900050;  -- Enumeration values directly visible.
   use C3900051;  -- Extended alert system abstraction.

   procedure Assign_Officer
     (Ma : in out Medium_Alert_Type; To : in Person_Enum)
   is
   begin
      Ma.Action_Officer := To;
   end Assign_Officer;

   procedure Handle (Ma : in out Medium_Alert_Type) is
   begin
      Handle (Low_Alert_Type (Ma));      -- Call parent's op (type conversion).
      Set_Level (Ma, 2);                 -- Call inherited operation.
      Assign_Officer (Ma, Duty_Officer); -- Call newly declared operation.
      Set_Display (Ma, Console);         -- Call inherited operation.
      Display (Ma);                      -- Call doubly inherited operation.
   end Handle;

   function Initial_Values_Okay (Ma : in Medium_Alert_Type) return Boolean is
   begin
      -- Call parent's operation (type conversion).
      return
        (Initial_Values_Okay (Low_Alert_Type (Ma)) and
         Ma.Action_Officer = Nobody);
   end Initial_Values_Okay;

   function Bad_Final_Values (Ma : in Medium_Alert_Type) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return
        (Get_Time (Ma) /= Alert_Time or Get_Display (Ma) /= Console or
         Get_Level (Ma) /= 2 or Ma.Action_Officer /= Duty_Officer);
   end Bad_Final_Values;

end C3900052;
