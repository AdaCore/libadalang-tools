----------------------------------------------------------------- C390003_3

with Ada.Tags;
with Report;
package body C390003_3 is -- Special_Trucks

   function "=" (A, B : Ada.Tags.Tag) return Boolean renames Ada.Tags."=";
   function "=" (A, B : Vehicle.Tc_Keys) return Boolean renames Vehicle."=";

   procedure Tc_Validate (It : Auto_Carrier; Key : Vehicle.Tc_Keys) is
   begin
      if Key /= Vehicle.Heavy then
         Report.Failed ("Expected Heavy Key");
      end if;
   end Tc_Validate;

   procedure Load (The_Car : in Motivators.Car; Onto : in out Auto_Carrier) is
   begin
      Onto.Load_Count                := Onto.Load_Count + 1;
      Onto.Payload (Onto.Load_Count) := The_Car;
   end Load;
   procedure Unload
     (The_Car :    out Motivators.Car;
      Off_Of  : in out Auto_Carrier)
   is
   begin
      The_Car           := Off_Of.Payload (Off_Of.Load_Count);
      Off_Of.Load_Count := Off_Of.Load_Count - 1;
   end Unload;

end C390003_3;
