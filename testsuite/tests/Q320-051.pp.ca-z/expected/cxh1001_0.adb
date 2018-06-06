-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body Cxh1001_0 is

   procedure Heap_Check
     (A_Value  : access Imp_H.Scalar_To_Normalize;
      A_Number : access Imp_H.Small_Number)
   is
      Value  : Num;
      Number : Integer;
   begin

      if A_Value.all'Valid then
         Value := Stn_2_Num (A_Value.all);
         if Imp_H.Default_For_Scalar_To_Normalize_Is_In_Range then
            if Imp_H.Scalar_To_Normalize'Val (Value) /=
              Imp_H.Default_For_Scalar_To_Normalize then
               Report.Failed
                 ("Implicit initial value for local variable is not " &
                  "the predicted value");
            end if;
         else
            if Value in
                0 ..
                      Imp_H.Scalar_To_Normalize'Pos
                        (Imp_H.Scalar_To_Normalize'Last)
            then
               Report.Failed
                 ("Implicit initial value for local variable is a " &
                  "value of the type");
            end if;
         end if;
      end if;

      if A_Number.all'Valid then
         Number := Integer (A_Number.all);
         if Imp_H.Default_For_Small_Number_Is_In_Range then
            if Global_Number /= Imp_H.Default_For_Small_Number then
               Report.Failed
                 ("Implicit initial value for number is not " &
                  "the predicted value");
            end if;
         else
            if Integer (Global_Number) in 0 .. Report.Ident_Int (Small_Last)
            then
               Report.Failed
                 ("Implicit initial value for number is a " &
                  "value of the type");
            end if;
         end if;
      end if;

   end Heap_Check;

   procedure Package_Check is
      Value  : Num;
      Number : Integer;
   begin

      if Global_Object'Valid then
         Value := Stn_2_Num (Global_Object);
         if Imp_H.Default_For_Scalar_To_Normalize_Is_In_Range then
            if Imp_H.Scalar_To_Normalize'Val (Value) /=
              Imp_H.Default_For_Scalar_To_Normalize then
               Report.Failed
                 ("Implicit initial value for local variable is not " &
                  "the predicted value");
            end if;
         else
            if Value in
                0 ..
                      Imp_H.Scalar_To_Normalize'Pos
                        (Imp_H.Scalar_To_Normalize'Last)
            then
               Report.Failed
                 ("Implicit initial value for local variable is a " &
                  "value of the type");
            end if;
         end if;
      end if;

      if Global_Number'Valid then
         Number := Integer (Global_Number);
         if Imp_H.Default_For_Small_Number_Is_In_Range then
            if Global_Number /= Imp_H.Default_For_Small_Number then
               Report.Failed
                 ("Implicit initial value for number is not " &
                  "the predicted value");
            end if;
         else
            if Integer (Global_Number) in 0 .. Report.Ident_Int (Small_Last)
            then
               Report.Failed
                 ("Implicit initial value for number is a " &
                  "value of the type");
            end if;
         end if;
      end if;

      Heap_Check (new Imp_H.Scalar_To_Normalize, new Imp_H.Small_Number);

   end Package_Check;

end Cxh1001_0;
