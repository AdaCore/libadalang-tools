-- CC3106B.ADA

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
--     CHECK THAT THE FORMAL PARAMETER DENOTES THE ACTUAL
--     IN AN INSTANTIATION.

-- HISTORY:
--     LDC 06/20/88  CREATED ORIGINAL TEST
--     EDWARD V. BERARD, 10 AUGUST 1990  ADDED CHECKS FOR MULTI-
--                                       DIMENSIONAL ARRAYS

with Report;

procedure Cc3106b is

begin  -- CC3106B

   Report.Test
     ("CC3106B",
      "CHECK THAT THE FORMAL PARAMETER DENOTES " &
      "THE ACTUAL IN AN INSTANTIATION");

   Local_Block :

   declare

      subtype Sm_Int is Integer range 0 .. 15;
      type Pck_Bol is array (5 .. 18) of Boolean;
      pragma Pack (Pck_Bol);

      Short_Start : constant := -100;
      Short_End   : constant := 100;
      type Short_Range is range Short_Start .. Short_End;

      subtype Really_Short is Short_Range range -9 .. 0;

      type Month_Type is
        (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);

      subtype First_Half is Month_Type range Jan .. Jun;

      type Day_Type is range 1 .. 31;
      type Year_Type is range 1_904 .. 2_050;
      type Date is record
         Month : Month_Type;
         Day   : Day_Type;
         Year  : Year_Type;
      end record;

      Today : Date := (Month => Aug, Day => 8, Year => 1_990);

      First_Date : Date := (Day => 6, Month => Jun, Year => 1_967);

      Wall_Date : Date := (Month => Nov, Day => 9, Year => 1_989);

      subtype First_Five is Character range 'A' .. 'E';

      type Three_Dimensional is
        array (Really_Short, First_Half, First_Five) of Date;

      Td_Array : Three_Dimensional :=
        (Three_Dimensional'Range =>
           (Three_Dimensional'Range (2) =>
              (Three_Dimensional'Range (3) => Today)));

      task type Tsk is
         entry Ent_1;
         entry Ent_2;
         entry Ent_3;
      end Tsk;

      generic

         type Gen_Type is (<>);
         Gen_Bolarr : in out Pck_Bol;
         Gen_Typ : in out Gen_Type;
         Gen_Tsk : in out Tsk;
         Test_Value : in Date;
         Test_Cube : in out Three_Dimensional;

      package P is
         procedure Gen_Proc1;
         procedure Gen_Proc2;
         procedure Gen_Proc3;
         procedure Array_Test;
      end P;

      Act_Bolarr : Pck_Bol := (others => False);
      Si         : Sm_Int  := 0;
      T          : Tsk;

      package body P is

         procedure Gen_Proc1 is
         begin  -- GEN_PROC1
            Gen_Bolarr (14) := Report.Ident_Bool (True);
            Gen_Typ         := Gen_Type'Val (4);
            if Act_Bolarr (14) /= True or Si /= Report.Ident_Int (4) then
               Report.Failed
                 ("VALUES ARE DIFFERENT THAN " & "INSTANTIATED VALUES");
            end if;
         end Gen_Proc1;

         procedure Gen_Proc2 is
         begin  -- GEN_PROC2
            if Gen_Bolarr (9) /= Report.Ident_Bool (True) or
              Gen_Type'Pos (Gen_Typ) /= Report.Ident_Int (2) then
               Report.Failed
                 ("VALUES ARE DIFFERENT THAN " &
                  "VALUES ASSIGNED IN THE MAIN " & "PROCEDURE");
            end if;
            Gen_Bolarr (18) := True;
            Gen_Typ         := Gen_Type'Val (9);
         end Gen_Proc2;

         procedure Gen_Proc3 is
         begin  -- GEN_PROC3
            Gen_Tsk.Ent_2;
         end Gen_Proc3;

         procedure Array_Test is
         begin  -- ARRAY_TEST

            Test_Cube (0, Jun, 'C') := Test_Value;

            if (Td_Array (0, Jun, 'C') /= Test_Value) or
              (Test_Cube (-5, Mar, 'A') /= Wall_Date) then
               Report.Failed
                 ("MULTI-DIMENSIONAL ARRAY VALUES ARE " &
                  "DIFFERENT THAN THE VALUES ASSIGNED " &
                  "IN THE MAIN AND ARRAY_TEST PROCEDURES.");
            end if;

         end Array_Test;

      end P;

      task body Tsk is
      begin  -- TSK
         accept Ent_1 do
            Report.Comment ("TASK ENTRY 1 WAS CALLED");
         end Ent_1;
         accept Ent_2 do
            Report.Comment ("TASK ENTRY 2 WAS CALLED");
         end Ent_2;
         accept Ent_3 do
            Report.Comment ("TASK ENTRY 3 WAS CALLED");
         end Ent_3;
      end Tsk;

      package Insta1 is new P (Gen_Type => Sm_Int, Gen_Bolarr => Act_Bolarr,
         Gen_Typ => Si, Gen_Tsk => T, Test_Value => First_Date,
         Test_Cube                      => Td_Array);

   begin  -- LOCAL_BLOCK

      Insta1.Gen_Proc1;
      Act_Bolarr (9) := True;
      Si             := 2;
      Insta1.Gen_Proc2;
      if Act_Bolarr (18) /= Report.Ident_Bool (True) or
        Si /= Report.Ident_Int (9) then
         Report.Failed
           ("VALUES ARE DIFFERENT THAN VALUES " &
            "ASSIGNED IN THE GENERIC PROCEDURE");
      end if;

      T.Ent_1;
      Insta1.Gen_Proc3;
      T.Ent_3;

      Td_Array (-5, Mar, 'A') := Wall_Date;
      Insta1.Array_Test;

   end Local_Block;

   Report.Result;

end Cc3106b;
