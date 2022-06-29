procedure Utils.Var_Length_Ints.Test is

   type My_Int is new Positive;

   package My_Int_Encodings is new Encodings (My_Int);
   use My_Int_Encodings;

   V : Octet_Vector;

   subtype Test_Cases is My_Int with
       Predicate => Test_Cases in 1 .. 1_000 | 16_000 .. 17_000 |
              2_097_100 .. 2_097_200 | 268_435_400 .. 268_435_500 |
              My_Int'Last - 100 .. My_Int'Last;

begin
   for X in Test_Cases loop
      Encode (V, X);
   end loop;

   declare
      Index  : Octet_Index := 1;
      Actual : My_Int;
   begin
      for Expected in Test_Cases loop
         Actual := Decode (V, Index);
         if Actual /= Expected then
            raise Program_Error;
         end if;
         Next (V, Index);
      end loop;

      if Index /= Last_Index (V) + 1 then
         raise Program_Error;
      end if;

      for Expected in reverse Test_Cases loop
         Prev (V, Index);
         Actual := Decode (V, Index);
         if Actual /= Expected then
            raise Program_Error;
         end if;
      end loop;

      if Index /= 1 then
         raise Program_Error;
      end if;
   end;
end Utils.Var_Length_Ints.Test;
