-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body Cxh1001_0.Cxh1001_1 is

   Childs_Object : Imp_H.Scalar_To_Normalize;  -- not initialized

   protected body Thingy is

      procedure Check_Embedded_Values is
      begin

         if Imp_H.Default_For_Scalar_To_Normalize_Is_In_Range then
            if Childs_Object /= Imp_H.Default_For_Scalar_To_Normalize then
               Report.Failed
                 ("Implicit initial value for child object is not " &
                  "the predicted value");
            end if;
         elsif Childs_Object'Valid
           and then Stn_2_Num (Childs_Object) in
             0 ..
                   Imp_H.Scalar_To_Normalize'Pos
                     (Imp_H.Scalar_To_Normalize'Last)
         then
            Report.Failed
              ("Implicit initial value for child object is a " &
               "value of the type");
         end if;

         if Imp_H.Default_For_Scalar_To_Normalize_Is_In_Range then
            if Hidden_Object /= Imp_H.Default_For_Scalar_To_Normalize then
               Report.Failed
                 ("Implicit initial value for protected package object " &
                  "is not the predicted value");
            end if;
         elsif Hidden_Object'Valid
           and then Stn_2_Num (Hidden_Object) in
             0 ..
                   Imp_H.Scalar_To_Normalize'Pos
                     (Imp_H.Scalar_To_Normalize'Last)
         then
            Report.Failed
              ("Implicit initial value for protected component " &
               "is a value of the type");
         end if;

         if Imp_H.Default_For_Small_Number_Is_In_Range then
            if Hidden_Number /= Imp_H.Default_For_Small_Number then
               Report.Failed
                 ("Implicit initial value for protected number " &
                  "is not the predicted value");
            end if;
         elsif Hidden_Number'Valid
           and then Hidden_Number in
             0 .. Imp_H.Small_Number (Report.Ident_Int (Small_Last))
         then
            Report.Failed
              ("Implicit initial value for protected number " &
               "is a value of the type");
         end if;

      end Check_Embedded_Values;

   end Thingy;

end Cxh1001_0.Cxh1001_1;
