-- C36205L.ADA

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
-- OBJECTIVE
--      FOR GENERIC PROCEDURES, CHECK THAT ATTRIBUTES GIVE THE
--      CORRECT VALUES FOR UNCONSTRAINED FORMAL PARAMETERS.
--      BASIC CHECKS OF ARRAY OBJECTS AND WHOLE ARRAYS PASSED AS
--      PARAMETERS TO GENERIC PROCEDURES

-- HISTORY
--      EDWARD V. BERARD, 9 AUGUST 1990
--      DAS   8 OCT 1990   ADDED OUT MODE PARAMETER TO GENERIC
--                         PROCEDURE TEST_PROCEDURE AND FORMAL
--                         GENERIC PARAMETER COMPONENT_VALUE.

with Report;

procedure C36205l is

   Short_Start : constant := -100;
   Short_End   : constant := 100;
   type Short_Range is range Short_Start .. Short_End;
   Short_Length : constant Natural := (Short_End - Short_Start + 1);

   Medium_Start : constant := 1;
   Medium_End   : constant := 100;
   type Medium_Range is range Medium_Start .. Medium_End;
   Medium_Length : constant Natural := (Medium_End - Medium_Start + 1);

   type Month_Type is
     (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
   type Day_Type is range 1 .. 31;
   type Year_Type is range 1_904 .. 2_050;
   type Date is record
      Month : Month_Type;
      Day   : Day_Type;
      Year  : Year_Type;
   end record;

   Today : Date := (Month => Aug, Day => 9, Year => 1_990);

   subtype Short_String is String (1 .. 5);

   Default_String : Short_String := "ABCDE";

   type First_Template is
     array (Short_Range range <>, Medium_Range range <>) of Date;

   type Second_Template is
     array (Month_Type range <>, Day_Type range <>) of Short_String;

   type Third_Template is
     array (Character range <>, Boolean range <>) of Day_Type;

   First_Array : First_Template (-10 .. 10, 27 .. 35) :=
     (-10 .. 10 => (27 .. 35 => Today));
   Second_Array : Second_Template (Jan .. Jun, 1 .. 25) :=
     (Jan .. Jun => (1 .. 25 => Default_String));
   Third_Array : Third_Template ('A' .. 'Z', False .. True) :=
     ('A' .. 'Z' => (False .. True => Day_Type (9)));

   Fourth_Array : First_Template (0 .. 27, 75 .. 100) :=
     (0 .. 27 => (75 .. 100 => Today));
   Fifth_Array : Second_Template (Jul .. Oct, 6 .. 10) :=
     (Jul .. Oct => (6 .. 10 => Default_String));
   Sixth_Array : Third_Template ('X' .. 'Z', True .. True) :=
     ('X' .. 'Z' => (True .. True => Day_Type (31)));

   generic

      type First_Index is (<>);
      type Second_Index is (<>);
      type Component_Type is private;
      type Unconstrained_Array is
        array (First_Index range <>, Second_Index range <>) of Component_Type;
      Component_Value : in Component_Type;

   procedure Test_Procedure (First : in     Unconstrained_Array;
      Ffifs : in First_Index; Ffils : in First_Index; Fsifs : in Second_Index;
      Fsils : in     Second_Index; Fflen : in Natural; Fslen : in Natural;
      Ffirt                        : in First_Index; Fsirt : in Second_Index;
      Second :    out Unconstrained_Array; Sfifs : in First_Index;
      Sfils : in First_Index; Ssifs : in Second_Index; Ssils : in Second_Index;
      Sflen : in     Natural; Sslen : in Natural; Sfirt : in First_Index;
      Ssirt                        : in     Second_Index; Remarks : in String);

   procedure Test_Procedure (First : in     Unconstrained_Array;
      Ffifs : in First_Index; Ffils : in First_Index; Fsifs : in Second_Index;
      Fsils : in     Second_Index; Fflen : in Natural; Fslen : in Natural;
      Ffirt                        : in First_Index; Fsirt : in Second_Index;
      Second :    out Unconstrained_Array; Sfifs : in First_Index;
      Sfils : in First_Index; Ssifs : in Second_Index; Ssils : in Second_Index;
      Sflen : in     Natural; Sslen : in Natural; Sfirt : in First_Index;
      Ssirt                        : in     Second_Index; Remarks : in String)
   is

   begin -- TEST_PROCEDURE

      if (First'First /= Ffifs) or (First'First (1) /= Ffifs) or
        (First'First (2) /= Fsifs) or (Second'First /= Sfifs) or
        (Second'First (1) /= Sfifs) or (Second'First (2) /= Ssifs) then
         Report.Failed ("PROBLEMS WITH 'FIRST. " & Remarks);
      end if;

      if (First'Last /= Ffils) or (First'Last (1) /= Ffils) or
        (First'Last (2) /= Fsils) or (Second'Last /= Sfils) or
        (Second'Last (1) /= Sfils) or (Second'Last (2) /= Ssils) then
         Report.Failed ("PROBLEMS WITH 'LAST. " & Remarks);
      end if;

      if (First'Length /= Fflen) or (First'Length (1) /= Fflen) or
        (First'Length (2) /= Fslen) or (Second'Length /= Sflen) or
        (Second'Length (1) /= Sflen) or (Second'Length (2) /= Sslen) then
         Report.Failed ("PROBLEMS WITH 'LENGTH. " & Remarks);
      end if;

      if (Ffirt not in First'Range (1)) or (Ffirt not in First'Range) or
        (Sfirt not in Second'Range (1)) or (Sfirt not in Second'Range) or
        (Fsirt not in First'Range (2)) or (Ssirt not in Second'Range (2)) then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE " & "ATTRIBUTE.  " & Remarks);
      end if;

      -- ASSIGN VALUES TO THE ARRAY PARAMETER OF MODE OUT
      for I in Second'Range (1) loop
         for J in Second'Range (2) loop
            Second (I, J) := Component_Value;
         end loop;
      end loop;

   end Test_Procedure;

   procedure First_Test_Procedure is new Test_Procedure
     (First_Index     => Short_Range, Second_Index => Medium_Range,
      Component_Type  => Date, Unconstrained_Array => First_Template,
      Component_Value => Today);

   procedure Second_Test_Procedure is new Test_Procedure
     (First_Index     => Month_Type, Second_Index => Day_Type,
      Component_Type  => Short_String, Unconstrained_Array => Second_Template,
      Component_Value => Default_String);

   procedure Third_Test_Procedure is new Test_Procedure
     (First_Index     => Character, Second_Index => Boolean,
      Component_Type  => Day_Type, Unconstrained_Array => Third_Template,
      Component_Value => Day_Type'First);

begin  -- C36205L

   Report.Test
     ("C36205L",
      "FOR GENERIC PROCEDURES, CHECK THAT " &
      "ATTRIBUTES GIVE THE CORRECT VALUES FOR " &
      "UNCONSTRAINED FORMAL PARAMETERS.  BASIC " &
      "CHECKS OF ARRAY OBJECTS AND WHOLE ARRAYS " &
      "PASSED AS PARAMETERS TO GENERIC PROCEDURES");

   First_Test_Procedure
     (First   => First_Array, Ffifs => -10, Ffils => 10, Fsifs => 27,
      Fsils   => 35, Fflen => 21, Fslen => 9, Ffirt => 0, Fsirt => 29,
      Second  => Fourth_Array, Sfifs => 0, Sfils => 27, Ssifs => 75,
      Ssils   => 100, Sflen => 28, Sslen => 26, Sfirt => 5, Ssirt => 100,
      Remarks => "FIRST_TEST_PROCEDURE");

   Second_Test_Procedure
     (First   => Second_Array, Ffifs => Jan, Ffils => Jun, Fsifs => 1,
      Fsils   => 25, Fflen => 6, Fslen => 25, Ffirt => Mar, Fsirt => 17,
      Second  => Fifth_Array, Sfifs => Jul, Sfils => Oct, Ssifs => 6,
      Ssils   => 10, Sflen => 4, Sslen => 5, Sfirt => Jul, Ssirt => 6,
      Remarks => "SECOND_TEST_PROCEDURE");

   Third_Test_Procedure
     (First   => Third_Array, Ffifs => 'A', Ffils => 'Z', Fsifs => False,
      Fsils   => True, Fflen => 26, Fslen => 2, Ffirt => 'T', Fsirt => True,
      Second  => Sixth_Array, Sfifs => 'X', Sfils => 'Z', Ssifs => True,
      Ssils   => True, Sflen => 3, Sslen => 1, Sfirt => 'Z', Ssirt => True,
      Remarks => "THIRD_TEST_PROCEDURE");

   Report.Result;

end C36205l;
