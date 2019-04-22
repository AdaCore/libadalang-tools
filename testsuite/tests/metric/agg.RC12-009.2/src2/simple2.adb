with Ada.Text_IO;
with F;

procedure Simple2 is

   ---------
   -- Inc --
   ---------

   function Inc (I : Integer) return Integer is
   begin
      return I + 1;
   end Inc;

   ---------
   -- Dec --
   ---------

   function Dec (I : Integer) return Integer is
   begin
      return I - 1;
   end Dec;

   ---------
   -- Bar --
   ---------

   procedure Bar is
   begin
      for J in 1 .. 42 loop
          for K in 1 .. 42 loop
             if J * K > 200 then
                return;
             end if;
          end loop;
      end loop;
   end Bar;

   ----------
   -- Main --
   ----------

   Number : constant Integer := 13;

begin
   Ada.Text_IO.Put_Line (Integer'Image (Inc (Number)));
   Ada.Text_IO.Put_Line (Integer'Image (Dec (Number)));

   F.Foo;
   Bar;
end Simple2;
