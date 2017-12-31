-- CC3224A.ADA

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
--     CHECK THAT A FORMAL ARRAY TYPE DENOTES ITS ACTUAL
--     PARAMETER, AND THAT OPERATIONS OF THE FORMAL TYPE ARE THOSE
--     IDENTIFIED WITH THE CORRESPONDING OPERATIONS OF THE ACTUAL TYPE.

-- HISTORY:
--     DHH 09/19/88  CREATED ORIGINAL TEST.
--     EDWARD V. BERARD, 14 AUGUST 1990  ADDED CHECKS FOR MULTI-
--                                       DIMENSIONAL ARRAYS
--     PWN 11/30/94 REMOVED 'BASE USE ILLEGAL IN ADA 9X.

with Report;

procedure Cc3224a is

   subtype Int is Integer range 1 .. 3;
   type Arr is array (1 .. 3) of Integer;
   type B_Arr is array (1 .. 3) of Boolean;

   Q : Arr;
   R : B_Arr;

   generic
      type T is array (Int) of Integer;
   package P is
      subtype Sub_T is T;
      X : Sub_T := (1, 2, 3);
   end P;

   generic
      type T is array (Int) of Boolean;
   package Bool is
      subtype Sub_T is T;
   end Bool;

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

   Td_Array        : Three_Dimensional;
   Second_Td_Array : Three_Dimensional;

   generic

      type Cube is array (Really_Short, First_Half, First_Five) of Date;

   package Td_Array_Package is

      subtype Sub_Cube is Cube;
      Test_3d_Array : Sub_Cube :=
        (Three_Dimensional'Range =>
           (Three_Dimensional'Range (2) =>
              (Three_Dimensional'Range (3) => Today)));

   end Td_Array_Package;

begin  -- CC3224A

   Report.Test
     ("CC3224A",
      "CHECK THAT A FORMAL ARRAY TYPE DENOTES " &
      "ITS ACTUAL PARAMETER, AND THAT OPERATIONS OF " &
      "THE FORMAL TYPE ARE THOSE IDENTIFIED WITH THE " &
      "CORRESPONDING OPERATIONS OF THE ACTUAL TYPE");

   One_Dimensional :

   declare

      package P1 is new P (Arr);

      type New_T is new P1.Sub_T;
      Obj_Newt : New_T;

   begin  -- ONE_DIMENSIONAL

      if New_T'First /= Arr'First then
         Report.Failed ("'FIRST ATTRIBUTE REPORT.FAILED");
      end if;

      if New_T'Last /= Arr'Last then
         Report.Failed ("'LAST ATTRIBUTE REPORT.FAILED");
      end if;

      if New_T'First (1) /= Arr'First (1) then
         Report.Failed ("'FIRST(N) ATTRIBUTE REPORT.FAILED");
      end if;

      if not (New_T'Last (1) = Arr'Last (1)) then
         Report.Failed ("'LAST(N) ATTRIBUTE REPORT.FAILED");
      end if;

      if 2 not in New_T'Range then
         Report.Failed ("'RANGE ATTRIBUTE REPORT.FAILED");
      end if;

      if 3 not in New_T'Range (1) then
         Report.Failed ("'RANGE(N) ATTRIBUTE REPORT.FAILED");
      end if;

      if New_T'Length /= Arr'Length then
         Report.Failed ("'LENGTH ATTRIBUTE REPORT.FAILED");
      end if;

      if New_T'Length (1) /= Arr'Length (1) then
         Report.Failed ("'LENGTH(N) ATTRIBUTE REPORT.FAILED");
      end if;

      Obj_Newt := (1, 2, 3);
      if Report.Ident_Int (3) /= Obj_Newt (3) then
         Report.Failed ("ASSIGNMENT REPORT.FAILED");
      end if;

      if New_T'(1, 2, 3) not in New_T then
         Report.Failed ("QUALIFIED EXPRESSION REPORT.FAILED");
      end if;

      Q := (1, 2, 3);
      if New_T (Q) /= Obj_Newt then
         Report.Failed ("EXPLICIT CONVERSION REPORT.FAILED");
      end if;

      if Q (1) /= Obj_Newt (1) then
         Report.Failed ("INDEXING REPORT.FAILED");
      end if;

      if (1, 2) /= Obj_Newt (1 .. 2) then
         Report.Failed ("SLICE REPORT.FAILED");
      end if;

      if (1, 2) & Obj_Newt (3) /= New_T (Q) then
         Report.Failed ("CATENATION REPORT.FAILED");
      end if;

      if not (P1.X in Arr) then
         Report.Failed ("FORMAL DOES NOT DENOTE ACTUAL");
      end if;

   end One_Dimensional;

   Boolean_One_Dimensional :

   declare

      package B1 is new Bool (B_Arr);

      type New_T is new B1.Sub_T;
      Obj_Newt : New_T;

   begin  -- BOOLEAN_ONE_DIMENSIONAL

      Obj_Newt := (True, True, True);
      R        := (True, True, True);

      if (New_T'((True, True, True)) xor Obj_Newt) /=
        New_T'((False, False, False)) then
         Report.Failed ("XOR REPORT.FAILED - BOOLEAN");
      end if;

      if (New_T'((False, False, True)) and Obj_Newt) /=
        New_T'((False, False, True)) then
         Report.Failed ("AND REPORT.FAILED - BOOLEAN");
      end if;

      if (New_T'((False, False, False)) or Obj_Newt) /=
        New_T'((True, True, True)) then
         Report.Failed ("OR REPORT.FAILED - BOOLEAN");
      end if;

   end Boolean_One_Dimensional;

   Three_Dimensional_Test :

   declare

      package Td is new Td_Array_Package (Cube => Three_Dimensional);

      type New_Cube is new Td.Sub_Cube;
      New_Cube_Object : New_Cube;

   begin  -- THREE_DIMENSIONAL_TEST

      if (New_Cube'First /= Three_Dimensional'First) or
        (New_Cube'First (1) /= Three_Dimensional'First) or
        (New_Cube'First (2) /= Three_Dimensional'First (2)) or
        (New_Cube'First (3) /= Three_Dimensional'First (3)) then
         Report.Failed
           ("PROBLEMS WITH 'FIRST FOR MULTI-" & "DIMENSIONAL ARRAYS.");
      end if;

      if (New_Cube'Last /= Three_Dimensional'Last) or
        (New_Cube'Last (1) /= Three_Dimensional'Last) or
        (New_Cube'Last (2) /= Three_Dimensional'Last (2)) or
        (New_Cube'Last (3) /= Three_Dimensional'Last (3)) then
         Report.Failed
           ("PROBLEMS WITH 'LAST FOR MULTI-" & "DIMENSIONAL ARRAYS.");
      end if;

      if (-5 not in New_Cube'Range) or (-3 not in New_Cube'Range (1)) or
        (Feb not in New_Cube'Range (2)) or ('C' not in New_Cube'Range (3)) then
         Report.Failed
           ("PROBLEMS WITH 'RANGE FOR MULTI-" & "DIMENSIONAL ARRAYS.");
      end if;

      if (New_Cube'Length /= Three_Dimensional'Length) or
        (New_Cube'Length (1) /= Three_Dimensional'Length) or
        (New_Cube'Length (2) /= Three_Dimensional'Length (2)) or
        (New_Cube'Length (3) /= Three_Dimensional'Length (3)) then
         Report.Failed
           ("PROBLEMS WITH 'LENGTH FOR MULTI-" & "DIMENSIONAL ARRAYS.");
      end if;

      New_Cube_Object :=
        (New_Cube'Range =>
           (New_Cube'Range (2) => (New_Cube'Range (3) => First_Date)));
      if First_Date /= New_Cube_Object (-3, Mar, 'D') then
         Report.Failed
           ("ASSIGNMENT FOR MULTI-DIMENSIONAL " & "ARRAYS FAILED.");
      end if;

      if New_Cube'
          (New_Cube'Range =>
             (New_Cube'Range (2) => (New_Cube'Range (3) => Wall_Date))) not in
          New_Cube then
         Report.Failed
           ("QUALIFIED EXPRESSION FOR MULTI-" & "DIMENSIONAL ARRAYS FAILED.");
      end if;

      Second_Td_Array :=
        (New_Cube'Range =>
           (New_Cube'Range (2) => (New_Cube'Range (3) => First_Date)));
      if New_Cube (Second_Td_Array) /= New_Cube_Object then
         Report.Failed
           ("EXPLICIT CONVERSION FOR MULTI-" & "DIMENSIONAL ARRAYS FAILED.");
      end if;

      if Second_Td_Array (-2, Feb, 'B') /= New_Cube_Object (-2, Feb, 'B') then
         Report.Failed ("INDEXING FOR MULTI-" & "DIMENSIONAL ARRAYS FAILED.");
      end if;

      if not (Td.Test_3d_Array in Three_Dimensional) then
         Report.Failed
           ("FORMAL MULTI-DIMENSIONAL ARRAY " & "DOES NOT DENOTE ACTUAL.");
      end if;

   end Three_Dimensional_Test;

   Report.Result;

end Cc3224a;
