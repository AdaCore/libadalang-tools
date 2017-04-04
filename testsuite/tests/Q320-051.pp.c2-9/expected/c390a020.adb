     --==================================================================--

package body C390a020 is

   use F390a00;  -- Alert system abstraction.

   function Level_Of (La : in Low_Alert_Type) return Integer is
   begin
      return (La.Level + 1);
   end Level_Of;

   procedure Handle (La : in out Low_Alert_Type) is
   begin
      Handle (Alert_Type (La));   -- Call parent's oper. (type conversion).
      La.Level      := Level_Of (La);  -- Call newly declared operation.
      La.Display_On := Teletype;
      Display (La);               -- Call inherited operation.
   end Handle;

end C390a020;
