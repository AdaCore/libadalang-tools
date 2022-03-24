procedure Main is
   Cant_Be_Pulled_Up : Natural;

begin
   declare
      --  Some comments

      type My_Natural_Range;

      --  Some comments
      --  A very natural range
      type My_Natural_Range is range 0 .. 10;
      --  A very natural Natural

      --  Some comments
      type Infiltrated_Type is new Integer;
      type My_Natural is new Natural range 0 .. 20; --  Some comments
      --  A very natural array of very natural Naturals
      type My_Natural_Array is array (My_Natural_Range) of My_Natural;
      --  A very natural array of very natural Naturals

      --  Some comments

      My_Natural_Value : My_Natural := 1;

      Natural_Array_1 : My_Natural_Array := (others => My_Natural_Value);

   begin
      null;
   end;
end Main;
