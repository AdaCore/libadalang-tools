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
      type My_Natural is new Natural range 0 .. 20; --  Some comments
      --  A very natural array of very natural Naturals
      type My_Natural_Array is array (My_Natural_Range) of My_Natural;
      --  A very natural array of very natural Naturals

      --  Some comments

      My_Natural_Value : My_Natural := 1;

      Natural_Array_1 : My_Natural_Array := (others => My_Natural_Value);
      Natural_Array_2 : My_Natural_Array := (others => My_Natural_Value);

      type Dummy_Type is new Float;

      procedure A_Natural_Procedure;
      --  A_Natural_Procedure

      procedure A_Natural_Procedure (Dummy : Dummy_Type);
      --  A_Natural_Procedure

      -------------------------
      -- A_Natural_Procedure --
      -------------------------

      procedure A_Natural_Procedure is
      begin
         Natural_Array_1 := (others => 0);
         A_Natural_Procedure (1.0);
      end A_Natural_Procedure;

      -------------------------
      -- A_Natural_Procedure --
      -------------------------

      procedure A_Natural_Procedure (Dummy : Dummy_Type) is
      begin
         Cant_Be_Pulled_Up := 0;
         Natural_Array_2 := (others => 0);
      end A_Natural_Procedure;

      type Another_Dummy_Type is new Float;

   begin
      A_Natural_Procedure (1.0);
      A_Natural_Procedure;
   end;
end Main;
