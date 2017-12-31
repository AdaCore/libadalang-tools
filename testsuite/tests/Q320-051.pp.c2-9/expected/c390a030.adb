     --==================================================================--

package body C390a030 is

   use F390a00;  -- Alert system abstraction.

   function Level_Of (La : in Low_Alert_Type) return Integer is
   begin
      return (La.Level + 1);
   end Level_Of;

   procedure Handle (La : in out Low_Alert_Type) is
   begin
      Handle (Alert_Type (La));   -- Call parent's operation (type conversion).
      La.Level      := Level_Of (La);  -- Call newly declared operation.
      La.Display_On := Teletype;
      Display (La);               -- Call inherited operation.
   end Handle;

   function Initial_Values_Okay (La : in Low_Alert_Type) return Boolean is
   begin
      return
        (La =
         (Arrival_Time => Default_Time,      -- Check "=" operator
          Display_On   => Null_Device,       -- availability.
          Level        => 0));               -- Aggregate with
   end Initial_Values_Okay;                              -- named associations.

   function Bad_Final_Values (La : in Low_Alert_Type) return Boolean is
   begin
      return (La /= (Alert_Time, Teletype, 1));          -- Check "/=" operator
      -- availability.
   end Bad_Final_Values;                                 -- Aggregate with
   -- positional assoc.

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
      -- Call parent's operation (type conversion).
      return
        (Initial_Values_Okay (Low_Alert_Type (Ma)) and
         Ma.Action_Officer = Nobody);
   end Initial_Values_Okay;

   function Bad_Final_Values (Ma : in Medium_Alert_Type) return Boolean is
   begin
      return not
        (Ma =
         (Arrival_Time   => Alert_Time,    -- Check "=" operator
          Display_On     => Console,       -- availability.
          Level          => 2,             -- Aggregate with
          Action_Officer => Duty_Officer));-- named associations.
   end Bad_Final_Values;

end C390a030;
