-- CC3007B.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--  CHECK THAT THE NAMES IN A GENERIC INSTANTIATION ARE STATICALLY IDENTIFIED
--  (I.E., BOUND) AT THE TEXTUAL POINT OF THE INSTANTIA- TION, AND ARE
--  BOUND BEFORE BEING "SUBSTITUTED" FOR THE COR- RESPONDING GENERIC
--  FORMAL PARAMETERS IN THE SPECIFICATION AND BODY TEMPLATES.
--
--  SEE AI-00365/05-BI-WJ.

-- HISTORY:
--      EDWARD V. BERARD, 15 AUGUST 1990
--      DAS   08 OCT 90   CHANGED INSTANTIATIONS TO USE VARIABLES
--                        M1 AND M2 IN THE FIRST_BLOCK INSTANTIA-
--                        TION AND TO ASSIGN THIRD_DATE AND
--                        FOURTH_DATE VALUES BEFORE AND AFTER THE
--                        SECOND_BLOCK INSTANTIATION.

with Report;

procedure Cc3007b is

   Incremented_Value : Natural := 0;

   type Month_Type is
     (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
   type Day_Type is range 1 .. 31;
   type Year_Type is range 1_904 .. 2_050;
   type Date is record
      Month : Month_Type;
      Day   : Day_Type;
      Year  : Year_Type;
   end record;

   type Date_Access is access Date;

   Today : Date := (Month => Aug, Day => 8, Year => 1_990);

   Christmas : Date := (Month => Dec, Day => 25, Year => 1_948);

   Wall_Date : Date := (Month => Nov, Day => 9, Year => 1_989);

   Birth_Date : Date := (Month => Oct, Day => 3, Year => 1_949);

   First_Due_Date : Date := (Month => Jan, Day => 23, Year => 1_990);

   Last_Due_Date : Date := (Month => Dec, Day => 20, Year => 1_990);

   This_Month : Month_Type := Aug;

   Stored_Record : Date := Today;

   Stored_Index : Month_Type := Aug;

   First_Date  : Date_Access := new Date'(Wall_Date);
   Second_Date : Date_Access := First_Date;

   Third_Date  : Date_Access := new Date'(Birth_Date);
   Fourth_Date : Date_Access := new Date'(Christmas);

   type Due_Dates is array (Month_Type range Jan .. Dec) of Date;
   Report_Dates : Due_Dates :=
     ((Jan, 23, 1_990), (Feb, 23, 1_990), (Mar, 23, 1_990), (Apr, 23, 1_990),
      (May, 23, 1_990), (Jun, 22, 1_990), (Jul, 23, 1_990), (Aug, 23, 1_990),
      (Sep, 24, 1_990), (Oct, 23, 1_990), (Nov, 23, 1_990), (Dec, 20, 1_990));

   generic

      Naturally : in Natural;
      First_Record : in out Date;
      Second_Record : in out Date;
      type Record_Pointer is access Date;
      Pointer : in out Record_Pointer;
      type Array_Type is array (Month_Type) of Date;
      This_Array : in out Array_Type;
      First_Array_Element : in out Date;
      Second_Array_Element : in out Date;
      Index_Element : in out Month_Type;
      Pointer_Test : in out Date;
      Another_Pointer_Test : in out Date;

   package Test_Actual_Parameters is

      procedure Evaluate_Function;
      procedure Check_Records;
      procedure Check_Access;
      procedure Check_Array;
      procedure Check_Array_Elements;
      procedure Check_Scalar;
      procedure Check_Pointers;

   end Test_Actual_Parameters;

   package body Test_Actual_Parameters is

      procedure Evaluate_Function is
      begin  -- EVALUATE_FUNCTION

         if (Incremented_Value = 0) or (Naturally /= Incremented_Value) then
            Report.Failed ("PROBLEMS EVALUATING FUNCTION " & "PARAMETER.");
         end if;

      end Evaluate_Function;

      procedure Check_Records is

         Store : Date;

      begin  -- CHECK_RECORDS

         if Stored_Record /= First_Record then
            Report.Failed ("PROBLEM WITH RECORD TYPES");
         else
            Stored_Record := Second_Record;
            Store         := First_Record;
            First_Record  := Second_Record;
            Second_Record := Store;
         end if;

      end Check_Records;

      procedure Check_Access is
      begin  -- CHECK_ACCESS

         if ((Incremented_Value / 2) * 2) /= Incremented_Value then
            if Pointer.all /= Date'(Wall_Date) then
               Report.Failed ("PROBLEM WITH ACCESS TYPES " & "- 1");
            else
               Pointer.all := Date'(Birth_Date);
            end if;
         else
            if Pointer.all /= Date'(Birth_Date) then
               Report.Failed ("PROBLEM WITH ACCESS TYPES " & "- 2");
            else
               Pointer.all := Date'(Wall_Date);
            end if;
         end if;

      end Check_Access;

      procedure Check_Array is

         Store : Date;

      begin  -- CHECK_ARRAY

         if ((Incremented_Value / 2) * 2) /= Incremented_Value then
            if This_Array (This_Array'First) /= First_Due_Date then
               Report.Failed ("PROBLEM WITH ARRAY TYPES - 1");
            else
               This_Array (This_Array'First) := Last_Due_Date;
               This_Array (This_Array'Last)  := First_Due_Date;
            end if;
         else
            if This_Array (This_Array'First) /= Last_Due_Date then
               Report.Failed ("PROBLEM WITH ARRAY TYPES - 2");
            else
               This_Array (This_Array'First) := First_Due_Date;
               This_Array (This_Array'Last)  := Last_Due_Date;
            end if;
         end if;

      end Check_Array;

      procedure Check_Array_Elements is

         Store : Date;

      begin  -- CHECK_ARRAY_ELEMENTS

         if ((Incremented_Value / 2) * 2) /= Incremented_Value then
            if (First_Array_Element.Month /= May) or
              (Second_Array_Element.Day /= 22) then
               Report.Failed ("PROBLEM WITH ARRAY ELEMENTS " & "- 1");
            else
               Store                := First_Array_Element;
               First_Array_Element  := Second_Array_Element;
               Second_Array_Element := Store;
            end if;
         else
            if (First_Array_Element.Month /= Jun) or
              (Second_Array_Element.Day /= 23) then
               Report.Failed ("PROBLEM WITH ARRAY ELEMENTS " & "- 2");
            else
               Store                := First_Array_Element;
               First_Array_Element  := Second_Array_Element;
               Second_Array_Element := Store;
            end if;
         end if;

      end Check_Array_Elements;

      procedure Check_Scalar is
      begin  -- CHECK_SCALAR

         if ((Incremented_Value / 2) * 2) /= Incremented_Value then
            if Index_Element /= Stored_Index then
               Report.Failed ("PROBLEM WITH INDEX TYPES - 1");
            else
               Index_Element := Month_Type'Succ (Index_Element);
               Stored_Index  := Index_Element;
            end if;
         else
            if Index_Element /= Stored_Index then
               Report.Failed ("PROBLEM WITH INDEX TYPES - 2");
            else
               Index_Element := Month_Type'Pred (Index_Element);
               Stored_Index  := Index_Element;
            end if;
         end if;

      end Check_Scalar;

      procedure Check_Pointers is

         Store : Date;

      begin  -- CHECK_POINTERS

         if ((Incremented_Value / 2) * 2) /= Incremented_Value then
            if (Pointer_Test /= Date'(Oct, 3, 1_949)) or
              (Another_Pointer_Test /= Date'(Dec, 25, 1_948)) then
               Report.Failed ("PROBLEM WITH POINTER TEST " & "- 1");
            else
               Store                := Pointer_Test;
               Pointer_Test         := Another_Pointer_Test;
               Another_Pointer_Test := Store;
            end if;
         else
            if (Pointer_Test /= Date'(Dec, 25, 1_948)) or
              (Another_Pointer_Test /= Date'(Oct, 3, 1_949)) then
               Report.Failed ("PROBLEM WITH POINTER TEST " & "- 2");
            else
               Store                := Pointer_Test;
               Pointer_Test         := Another_Pointer_Test;
               Another_Pointer_Test := Store;
            end if;
         end if;

      end Check_Pointers;

   end Test_Actual_Parameters;

   function Inc return Natural is
   begin  -- INC
      Incremented_Value := Natural'Succ (Incremented_Value);
      return Incremented_Value;
   end Inc;

begin  -- CC3007B

   Report.Test
     ("CC3007B",
      "CHECK THAT THE NAMES IN A GENERIC " &
      "INSTANTIATION ARE STAICALLY IDENTIFIED (I.E., " &
      "BOUND) AT THE TEXTUAL POINT OF THE INSTANTIATION" &
      ", AND ARE BOUND BEFORE BEING SUBSTITUTED FOR " &
      "THE CORRESPONDING GENERIC FORMAL PARAMETERS IN " &
      "THE SPECIFICATION AND BODY TEMPLATES.  " & "SEE AI-00365/05-BI-WJ.");

   First_Block :

   declare

      M1 : Month_Type := May;
      M2 : Month_Type := Jun;

      package New_Test_Actual_Parameters is new Test_Actual_Parameters
        (Naturally => Inc, First_Record => Today, Second_Record => Christmas,
         Record_Pointer       => Date_Access, Pointer => Second_Date,
         Array_Type           => Due_Dates, This_Array => Report_Dates,
         First_Array_Element  => Report_Dates (M1),
         Second_Array_Element => Report_Dates (M2),
         Index_Element        => This_Month, Pointer_Test => Third_Date.all,
         Another_Pointer_Test => Fourth_Date.all);

   begin  -- FIRST_BLOCK

      Report.Comment ("ENTERING FIRST BLOCK");
      New_Test_Actual_Parameters.Evaluate_Function;
      New_Test_Actual_Parameters.Check_Scalar;
      M1 := Sep;
      M2 := Oct;
      -- NEW_TEST_ACTUAL_PARAMETERS SHOULD USE THE PREVIOUS VALUES OF MAY AND
      -- JUN.
      New_Test_Actual_Parameters.Check_Array;
      New_Test_Actual_Parameters.Check_Array_Elements;
      New_Test_Actual_Parameters.Check_Access;
      New_Test_Actual_Parameters.Check_Records;
      New_Test_Actual_Parameters.Check_Pointers;

   end First_Block;

   Second_Block :

   declare

      Save_Third_Date  : Date_Access := Third_Date;
      Save_Fourth_Date : Date_Access := Fourth_Date;

      package New_Test_Actual_Parameters is new Test_Actual_Parameters
        (Naturally => Inc, First_Record => Today, Second_Record => Christmas,
         Record_Pointer       => Date_Access, Pointer => Second_Date,
         Array_Type           => Due_Dates, This_Array => Report_Dates,
         First_Array_Element  => Report_Dates (May),
         Second_Array_Element => Report_Dates (Jun),
         Index_Element        => This_Month, Pointer_Test => Third_Date.all,
         Another_Pointer_Test => Fourth_Date.all);

   begin  -- SECOND_BLOCK

      Report.Comment ("ENTERING SECOND BLOCK");
      New_Test_Actual_Parameters.Evaluate_Function;
      New_Test_Actual_Parameters.Check_Scalar;
      New_Test_Actual_Parameters.Check_Array;
      New_Test_Actual_Parameters.Check_Array_Elements;
      New_Test_Actual_Parameters.Check_Access;
      New_Test_Actual_Parameters.Check_Records;

      Third_Date  := new Date'(Jul, 13, 1_951);
      Fourth_Date := new Date'(Jul, 4, 1_976);
      New_Test_Actual_Parameters.Check_Pointers;
      Third_Date  := Save_Third_Date;
      Fourth_Date := Save_Fourth_Date;

   end Second_Block;

   Report.Result;

end Cc3007b;
