--==================================================================--

package body C730002_5 is

   function Tc_Specialist (E : Nuclear_Series) return Specialist_Id is
   begin
      return E.Specialist;
   end Tc_Specialist;

   function Tc_Personnel_Required (E : Nuclear_Series) return Personnel_Type is
   begin
      return E.Personnel_Required;
   end Tc_Personnel_Required;

   function Tc_Time_Required (E : Nuclear_Series) return Hours_Type is
   begin
      return E.Ave_Repair_Time;
   end Tc_Time_Required;

   -- Dispatching subprogram.
   procedure Maintain_The_Engine (The_Engine : in out Engine_Type'Class) is
   begin
      Routine_Maintenance (The_Engine);
   end Maintain_The_Engine;

end C730002_5;
