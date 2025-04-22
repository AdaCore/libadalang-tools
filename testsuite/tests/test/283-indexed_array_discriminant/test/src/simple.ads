package Simple
with SPARK_Mode
is
   type Int_Array is array (Positive range <>) of Integer;

   type My_Record (Size : Positive) is record
      Data : Int_Array (1 .. Size);
   end record;

   procedure Check_Record (R : My_Record);

end Simple;
