----------------------------------------------------------------- C390003_2

with Ada.Tags;
with Report;
package body C390003_2 is -- Motivators

   function "=" (A, B : Ada.Tags.Tag) return Boolean renames Ada.Tags."=";
   function "=" (A, B : Vehicle.Tc_Keys) return Boolean renames Vehicle."=";

   procedure Tc_Validate (It : Motorcycle; Key : Vehicle.Tc_Keys) is
   begin
      if Key /= Vehicle.Mc then
         Report.Failed ("Expected MC Key");
      end if;
   end Tc_Validate;

   procedure Tc_Validate (It : Tandem; Key : Vehicle.Tc_Keys) is
   begin
      if Key /= Vehicle.Tand then
         Report.Failed ("Expected Tand Key");
      end if;
   end Tc_Validate;

   procedure Tc_Validate (It : Car; Key : Vehicle.Tc_Keys) is
   begin
      if Key /= Vehicle.Car then
         Report.Failed ("Expected Car Key");
      end if;
   end Tc_Validate;

   procedure Tc_Validate (It : Truck; Key : Vehicle.Tc_Keys) is
   begin
      if Key /= Vehicle.Truk then
         Report.Failed ("Expected Truk Key");
      end if;
   end Tc_Validate;
end C390003_2; -- Motivators;
