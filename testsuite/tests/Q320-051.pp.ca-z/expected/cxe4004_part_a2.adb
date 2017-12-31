-----------------------------------------------------------------------------

with Report;
package body Cxe4004_Part_A2 is

   procedure Call_With_2 (T : Integer_Vector) is
      Expected : constant Integer_Vector (1 .. 2) := (2, 2);
   begin
      if T /= Expected then
         Report.Failed
           ("expected 2 2s but received:" & Integer'Image (T (T'First)) &
            " length:" & Integer'Image (T'Length));
      end if;
   end Call_With_2;

   procedure Call_With_3 (T : Integer_Vector) is
      Expected : constant Integer_Vector (1 .. 3) := (3, 3, 3);
   begin
      if T /= Expected then
         Report.Failed
           ("expected 3 3s but received:" & Integer'Image (T (T'First)) &
            " length:" & Integer'Image (T'Length));
      end if;
   end Call_With_3;

end Cxe4004_Part_A2;
