----------------------------------------------------------------------

with F393a00_2;
with F393a00_3;
package F393a00_4 is
   type Mill is new F393a00_2.Windmill with private;

   procedure Swap (A, B : in out Mill);
   function Create return Mill;
   procedure Stop (It : in out Mill);
private
   type Mill is new F393a00_2.Windmill with record
      Pump : F393a00_3.Pump := F393a00_3.Create;
   end record;
end F393a00_4;
