-----------------------------------------------------------------------------

with F393a00_0;
package body C393a03_1 is

   procedure Swap (A, B : in out Modular_Object) is
      T : constant Modular_Object := B;
   begin
      F393a00_0.Tc_Touch ('1');
      B := A;
      A := T;
   end Swap;

   procedure Clear (It : in out Modular_Object) is
   begin
      F393a00_0.Tc_Touch ('2');
      null;
   end Clear;

   procedure Set_Max (It : in out Modular_Object; Value : Natural) is
   begin
      F393a00_0.Tc_Touch ('3');
      It.Max_Value := Value;
   end Set_Max;

   function Create return Modular_Object is
      Amo : Modular_Object;
   begin
      F393a00_0.Tc_Touch ('4');
      Amo.Max_Value := Natural'Last;
      return Amo;
   end Create;

end C393a03_1;
