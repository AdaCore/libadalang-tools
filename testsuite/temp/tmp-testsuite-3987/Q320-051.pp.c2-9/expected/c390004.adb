with Report;
with Ada.Tags;
with C390004_1;
procedure C390004 is
   package Dmv renames C390004_1;

   The_Vehicle     : aliased Dmv.Vehicle;
   The_Car         : aliased Dmv.Car;
   The_Convertible : aliased Dmv.Convertible;
   The_Jeep        : aliased Dmv.Jeep;

   type C_Reference is access all Dmv.Car'Class;
   type V_Reference is access all Dmv.Vehicle'Class;

   Designator : V_Reference;
   Storage    : Natural;

   procedure Valet (It : in out Dmv.Vehicle'Class) is
   begin
      Dmv.Park (It);
   end Valet;

   procedure Tc_Match
     (Object : Dmv.Vehicle'Class;
      Taglet : Ada.Tags.Tag;
      Where  : String)
   is
      use Ada.Tags;
   begin
      if Object'Tag /= Taglet then
         Report.Failed ("Tag mismatch: " & Where);
      end if;
   end Tc_Match;

   procedure Parking_Validation (It : Dmv.Vehicle; Tc_Message : String) is
   begin
      if Dmv.Wheels (It) /= 1 or not It.Parked then
         Report.Failed ("Failed Vehicle " & Tc_Message);
      end if;
   end Parking_Validation;

   procedure Parking_Validation (It : Dmv.Car; Tc_Message : String) is
   begin
      if Dmv.Wheels (It) /= 2 or Dmv.Passengers (It) /= 0 or not It.Parked then
         Report.Failed ("Failed Car " & Tc_Message);
      end if;
   end Parking_Validation;

   procedure Parking_Validation (It : Dmv.Convertible; Tc_Message : String) is
   begin
      if Dmv.Wheels (It) /= 3 or
        Dmv.Passengers (It) /= 0 or
        not Dmv.Top_Up (It) or
        not It.Parked
      then
         Report.Failed ("Failed Convertible " & Tc_Message);
      end if;
   end Parking_Validation;

   procedure Parking_Validation (It : Dmv.Jeep; Tc_Message : String) is
   begin
      if Dmv.Wheels (It) /= 4 or
        Dmv.Passengers (It) /= 0 or
        not Dmv.Top_Up (It) or
        not Dmv.Windshield_Up (It) or
        not It.Parked
      then
         Report.Failed ("Failed Jeep " & Tc_Message);
      end if;
   end Parking_Validation;

   function Wash
     (It        : V_Reference;
      Tc_Expect : Ada.Tags.Tag) return Dmv.Vehicle'Class
   is
      This_Machine : Dmv.Vehicle'Class := It.all;
   begin
      Tc_Match (It.all, Tc_Expect, "Class-wide object in Wash");
      Storage := Dmv.Wheels (This_Machine);
      return This_Machine;
   end Wash;

   function Wash
     (It        : C_Reference;
      Tc_Expect : Ada.Tags.Tag) return Dmv.Car'Class
   is
      This_Machine : Dmv.Car'Class := It.all;
   begin
      Tc_Match (It.all, Tc_Expect, "Class-wide object in Wash");
      Storage := Dmv.Wheels (This_Machine);
      return This_Machine;
   end Wash;

begin

   Report.Test
     ("C390004",
      "Check that the tags of allocated objects " &
      "correctly identify the type of the allocated " &
      "object.  Check that tags resulting from " &
      "normal and view conversions.  Check tags of " &
      "accessed values designating aliased objects. " &
      "Check function result tags");

   Dmv.Set_Wheels (The_Vehicle, 1);
   Dmv.Set_Wheels (The_Car, 2);
   Dmv.Set_Wheels (The_Convertible, 3);
   Dmv.Set_Wheels (The_Jeep, 4);

   Valet (The_Vehicle);
   Valet (The_Car);
   Valet (The_Convertible);
   Valet (The_Jeep);

   Parking_Validation (The_Vehicle, "setup");
   Parking_Validation (The_Car, "setup");
   Parking_Validation (The_Convertible, "setup");
   Parking_Validation (The_Jeep, "setup");

-- Check that the tags of allocated objects correctly identify the type
-- of the allocated object.

   Designator := new Dmv.Vehicle;
   Dmv.Tc_Check (Designator.all, Dmv.T_Veh);
   Tc_Match (Designator.all, Dmv.Vehicle'Tag, "allocated Vehicle");

   Designator := new Dmv.Car;
   Dmv.Tc_Check (Designator.all, Dmv.T_Car);
   Tc_Match (Designator.all, Dmv.Car'Tag, "allocated Car");

   Designator := new Dmv.Convertible;
   Dmv.Tc_Check (Designator.all, Dmv.T_Con);
   Tc_Match (Designator.all, Dmv.Convertible'Tag, "allocated Convertible");

   Designator := new Dmv.Jeep;
   Dmv.Tc_Check (Designator.all, Dmv.T_Jep);
   Tc_Match (Designator.all, Dmv.Jeep'Tag, "allocated Jeep");

-- Check that view conversion causes the correct dispatch
   Dmv.Tc_Check (Dmv.Vehicle (The_Jeep), Dmv.T_Veh);
   Dmv.Tc_Check (Dmv.Car (The_Jeep), Dmv.T_Car);
   Dmv.Tc_Check (Dmv.Convertible (The_Jeep), Dmv.T_Con);

-- And that view conversion does not change the tag
   Tc_Match (Dmv.Vehicle (The_Jeep), Dmv.Jeep'Tag, "View Conv Veh");
   Tc_Match (Dmv.Car (The_Jeep), Dmv.Jeep'Tag, "View Conv Car");
   Tc_Match (Dmv.Convertible (The_Jeep), Dmv.Jeep'Tag, "View Conv Jep");

-- Check that the tags of accessed values designating aliased objects
-- correctly identify the type of the object.
   Designator := The_Vehicle'Access;
   Dmv.Tc_Check (Designator.all, Dmv.T_Veh);
   Tc_Match (Designator.all, Dmv.Vehicle'Tag, "aliased Vehicle");

   Designator := The_Car'Access;
   Dmv.Tc_Check (Designator.all, Dmv.T_Car);
   Tc_Match (Designator.all, Dmv.Car'Tag, "aliased Car");

   Designator := The_Convertible'Access;
   Dmv.Tc_Check (Designator.all, Dmv.T_Con);
   Tc_Match (Designator.all, Dmv.Convertible'Tag, "aliased Convertible");

   Designator := The_Jeep'Access;
   Dmv.Tc_Check (Designator.all, Dmv.T_Jep);
   Tc_Match (Designator.all, Dmv.Jeep'Tag, "aliased Jeep");

-- Check that the tag of a function result correctly evaluates.
-- Check this for class-wide functions.  The tag of a class-wide
-- function result should be the tag appropriate to the actual value
-- returned, not the tag of the ancestor type.
   Function_Check : declare
      A_Vehicle     : V_Reference := new Dmv.Vehicle'(The_Vehicle);
      A_Car         : C_Reference := new Dmv.Car'(The_Car);
      A_Convertible : C_Reference := new Dmv.Convertible'(The_Convertible);
      A_Jeep        : C_Reference := new Dmv.Jeep'(The_Jeep);
   begin
      Dmv.Unpark (A_Vehicle.all);
      Dmv.Load_Passengers (A_Car.all, 5);
      Dmv.Load_Passengers (A_Convertible.all, 6);
      Dmv.Load_Passengers (A_Jeep.all, 7);
      Dmv.Lower_Top (Dmv.Convertible (A_Convertible.all));
      Dmv.Lower_Top (Dmv.Jeep (A_Jeep.all));
      Dmv.Lower_Windshield (Dmv.Jeep (A_Jeep.all));

      if Dmv.Wheels (Wash (A_Jeep, Dmv.Jeep'Tag)) /= 4 or Storage /= 4 then
         Report.Failed ("Did not correctly wash Jeep");
      end if;

      if Dmv.Wheels (Wash (A_Convertible, Dmv.Convertible'Tag)) /= 3 or
        Storage /= 3
      then
         Report.Failed ("Did not correctly wash Convertible");
      end if;

      if Dmv.Wheels (Wash (A_Car, Dmv.Car'Tag)) /= 2 or Storage /= 2 then
         Report.Failed ("Did not correctly wash Car");
      end if;

      if Dmv.Wheels (Wash (A_Vehicle, Dmv.Vehicle'Tag)) /= 1 or
        Storage /= 1
      then
         Report.Failed ("Did not correctly wash Vehicle");
      end if;

   end Function_Check;

   Report.Result;
end C390004;
