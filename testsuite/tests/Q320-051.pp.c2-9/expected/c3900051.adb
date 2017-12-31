     --==================================================================--

with Ada.Calendar;
pragma Elaborate (Ada.Calendar);

package body C3900051 is  -- Extended alert system abstraction.

   use C3900050;  -- Alert system abstraction.

   procedure Set_Level (La : in out Low_Alert_Type; L : in Integer) is
   begin
      La.Level := L;
   end Set_Level;

   procedure Handle (La : in out Low_Alert_Type) is
   begin
      Handle (Alert_Type (La));   -- Call parent's operation (type conversion).
      Set_Level (La, 1);          -- Call newly declared operation.
      Set_Display
        (Alert_Type (La),
         Teletype);     -- Call parent's operation (type conversion).
      Display (La);
   end Handle;

   function Get_Level (La : Low_Alert_Type) return Integer is
   begin
      return La.Level;
   end Get_Level;

   function Initial_Values_Okay (La : in Low_Alert_Type) return Boolean is
   begin
      -- Call parent's operation (type conversion).
      return (Initial_Values_Okay (Alert_Type (La)) and La.Level = 0);
   end Initial_Values_Okay;

   function Bad_Final_Values (La : in Low_Alert_Type) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return
        (Get_Time (La) /= Alert_Time or Get_Display (La) /= Teletype or
         La.Level /= 1);
   end Bad_Final_Values;

end C3900051;
