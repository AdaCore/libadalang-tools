pragma Ada_2012;
package body Number.Operations is

   function Add (L, R : Number_Type) return Number_Type is
      X : constant Integer := Integer (L) + Integer (R);
   begin
      if X > 100 or X < -100 then
         Overflow := True;
         return 0;
      end if;

      return L + R;
   end Add;

   function Multiply (L, R : Number_Type) return Number_Type is
      X : constant Integer := Integer (L) * Integer (R);
   begin
      if X > 100 or X < -100 then
         Overflow := True;
         return 0;
      end if;

      return L * R;
   end Multiply;

end Number.Operations;
