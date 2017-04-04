with C3a0013_1;
package C3a0013_2 is
   type Car is new C3a0013_1.Vehicle with private;
   procedure Tc_Validate (It : Car; Tc_Id : Character);
   function Gear_Factor (It : Car) return Natural;
private
   type Car is new C3a0013_1.Vehicle with record
      Displacement : Natural;
   end record;
end C3a0013_2;
