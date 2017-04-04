with C3a0013_1;
package C3a0013_3 is
   type Truck is new C3a0013_1.Vehicle with private;
   procedure Tc_Validate (It : Truck; Tc_Id : Character);
   function Gear_Factor (It : Truck) return Natural;
private
   type Truck is new C3a0013_1.Vehicle with record
      Displacement : Natural;
   end record;
end C3a0013_3;
