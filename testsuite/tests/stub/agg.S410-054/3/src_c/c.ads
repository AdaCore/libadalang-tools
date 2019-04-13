package C is
  D : Boolean;
  S : String := "ololo";

  I : Integer;
   type My_Integer_Range_10 is range 1 .. 10;
   procedure Do_Something (Param_1, Param_2 : My_Integer_Range_10);
   function Something_Else (Param_1, Param_2 : My_Integer_Range_10) return Boolean;
end C;
