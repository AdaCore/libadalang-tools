-- CC1224A.ADA

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
-- OBJECTIVE:
--     FOR ARRAY TYPES WITH A NONLIMITED COMPONENT TYPE (OF A FORMAL
--     AND NONFORMAL GENERIC TYPE), CHECK THAT THE FOLLOWING OPERATIONS
--     ARE IMPLICITY DECLARED AND ARE, THEREFORE, AVAILABLE WITHIN THE
--     GENERIC UNIT: ASSIGNMENT, THE OPERATION ASSOCIATED WITH
--     AGGREGATE NOTATION, MEMBERSHIP TESTS, THE OPERATION ASSOCIATED
--     WITH INDEXED COMPONENTS, QUALIFICATION, EXPLICIT CONVERSION,
--     'SIZE, 'ADDRESS, 'FIRST, 'FIRST (N), 'LAST, 'LAST (N),
--     'RANGE, 'RANGE (N), 'LENGTH, 'LENGTH (N).

-- HISTORY:
--     R.WILLIAMS  10/6/86
--     EDWARD V. BERARD  8/10/90  ADDED CHECKS FOR MULTI-DIMENSIONAL
--                                ARRAYS
--     LDC  10/10/90  CHANGED DECLARATIONS OF AD1 - AD6 TO PROCEDURE
--                    CALLS OF FA1 - FA6 TO ADDRESS_CHECK AS SUGGESTED
--                    BY THE CRG.
--     PWN  01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System;
with Report;

procedure Cc1224a is

   Short_Start : constant := -10;
   Short_End   : constant := 10;

   type Short_Range is range Short_Start .. Short_End;
   Short_Length : constant Natural := (Short_End - Short_Start + 1);

   Medium_Start : constant := 1;
   Medium_End   : constant := 15;

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

   Today : Date := (Aug, 10, 1_990);

   type First_Template is
     array (Short_Range range <>, Medium_Range range <>) of Date;

   type Second_Template is array (Short_Range, Medium_Range) of Date;

   First_Array  : First_Template (-10 .. 10, 6 .. 10);
   Second_Array : First_Template (0 .. 7, 1 .. 15);
   Third_Array  : Second_Template;
   Fourth_Array : Second_Template;

   subtype Subint is
     Integer range Report.Ident_Int (1) .. Report.Ident_Int (6);

   type Arra is array (Subint) of Subint;
   A1 : Arra := (Report.Ident_Int (1) .. Report.Ident_Int (6) => 1);
   A2 : Arra := (A1'Range => 2);

   type Arrb is array (Subint range <>) of Date;
   A3 : Arrb (1 .. 6) :=
     (Report.Ident_Int (1) .. Report.Ident_Int (6) => Today);

   type Arrc is array (Subint range <>, Subint range <>) of Subint;
   A4 : constant Arrc := (1 .. 6 => (1 .. 6 => 4));

   type Arrd is array (Subint, Subint) of Subint;
   A5 : Arrd := (A4'Range (1) => (A4'Range (2) => 5));

   type Arre is array (Subint) of Date;
   A6 : Arre := (A1'Range => Today);

   function "="
     (Left : in System.Address; Right : in System.Address)
      return Boolean renames
     System."=";

   generic

      type T1 is (<>);
      type T2 is private;
      X2 : T2;

      type Farr1 is array (Subint) of T1;
      Fa1 : Farr1;

      type Farr2 is array (Subint) of Subint;
      Fa2 : Farr2;

      type Farr3 is array (Subint range <>) of T2;
      Fa3 : Farr3;

      type Farr4 is array (Subint range <>, Subint range <>) of T1;
      Fa4 : Farr4;

      type Farr5 is array (Subint, Subint) of Subint;
      Fa5 : Farr5;

      type Farr6 is array (T1) of T2;
      Fa6 : Farr6;

      type Farr7 is array (T1) of T2;
      Fa7 : Farr7;

   procedure P;

   generic

      type First_Index is (<>);
      type Second_Index is (<>);
      type Unconstrained_Array is
        array (First_Index range <>, Second_Index range <>) of Date;

   procedure Test_Procedure
     (First : in Unconstrained_Array; Ffifs : in First_Index;
      Ffils : in First_Index; Fsifs : in Second_Index; Fsils : in Second_Index;
      Fflen : in Natural; Fslen : in Natural; Ffirt : in First_Index;
      Fsirt : in Second_Index; Second : in Unconstrained_Array;
      Sfifs : in First_Index; Sfils : in First_Index; Ssifs : in Second_Index;
      Ssils : in Second_Index; Sflen : in Natural; Sslen : in Natural;
      Sfirt : in First_Index; Ssirt : in Second_Index; Remarks : in String);
   generic

      type First_Index is (<>);
      type Second_Index is (<>);
      type Component_Type is private;
      type Constrained_Array is
        array (First_Index, Second_Index) of Component_Type;

   procedure Ctest_Procedure
     (First : in Constrained_Array; Ffirt : in First_Index;
      Fsirt : in Second_Index; Second : in Constrained_Array;
      Sfirt : in First_Index; Ssirt : in Second_Index; Remarks : in String);

   procedure P is

      In1 : Integer := Fa1'Size;
      In2 : Integer := Fa2'Size;
      In3 : Integer := Fa3'Size;
      In4 : Integer := Fa4'Size;
      In5 : Integer := Fa5'Size;
      In6 : Integer := Fa6'Size;

      B1 : Farr1;

      B2 : Farr2;

      subtype Sarr3 is Farr3 (Fa3'Range);
      B3 : Sarr3;

      subtype Sarr4 is Farr4 (Fa4'Range (1), Fa4'Range (2));
      B4 : Sarr4;

      B5 : Farr5;

      B6 : Farr6;

      procedure Address_Check (Address : System.Address) is

      begin
         if Report.Equal (1, Report.Ident_Int (2)) then
            Report.Comment ("DON'T OPTIMIZE OUT ADDRESS_CHECK");
         end if;
      end Address_Check;

   begin  -- P

      Address_Check (Fa1'Address);
      Address_Check (Fa2'Address);
      Address_Check (Fa3'Address);
      Address_Check (Fa4'Address);
      Address_Check (Fa5'Address);
      Address_Check (Fa6'Address);

      B1 := Fa1;

      if B1 /= Farr1 (Fa1) then
         Report.Failed ("INCORRECT RESULTS - 1");
      end if;

      B2 := Fa2;

      if B2 /= Farr2 (A2) then
         Report.Failed ("INCORRECT RESULTS - 2");
      end if;

      B3 := Fa3;

      if B3 /= Farr3 (Fa3) then
         Report.Failed ("INCORRECT RESULTS - 3");
      end if;

      B4 := Fa4;

      if B4 /= Farr4 (Fa4) then
         Report.Failed ("INCORRECT RESULTS - 4");
      end if;

      B5 := Fa5;

      if B5 /= Farr5 (A5) then
         Report.Failed ("INCORRECT RESULTS - 5");
      end if;

      B6 := Fa6;

      if B6 /= Farr6 (Fa6) then
         Report.Failed ("INCORRECT RESULTS - 6");
      end if;

      if Fa7 /= Farr7 (Fa6) then
         Report.Failed ("INCORRECT RESULTS - 7");
      end if;

      B1 := Farr1'(Fa1'Range => T1'Val (1));

      if B1 (1) /= Fa1 (1) then
         Report.Failed ("INCORRECT RESULTS - 8");
      end if;

      B1 := Farr1'(1 => T1'Val (1), 2 => T1'Val (1), 3 .. 6 => T1'Val (2));

      if B1 (1) /= Fa1 (1) then
         Report.Failed ("INCORRECT RESULTS - 9");
      end if;

      B2 := Farr2'(Fa2'Range => 2);

      if B2 (2) /= Fa2 (2) then
         Report.Failed ("INCORRECT RESULTS - 10");
      end if;

      B3 := Farr3'(1 | 2 | 3 => X2, 4 | 5 | 6 => X2);

      if B3 (3) /= Fa3 (3) then
         Report.Failed ("INCORRECT RESULTS - 11");
      end if;

      B4 := Farr4'(Fa5'Range (1) => (Fa5'Range (2) => T1'Val (4)));

      if B4 (4, 4) /= Fa4 (4, 4) then
         Report.Failed ("INCORRECT RESULTS - 12");
      end if;

      B5 :=
        Farr5'(Report.Ident_Int (1) .. Report.Ident_Int (6) => (1 .. 6 => 5));

      if B5 (5, 5) /= Fa5 (5, 5) then
         Report.Failed ("INCORRECT RESULTS - 13");
      end if;

      B6 := Farr6'(Fa6'Range => X2);

      if B6 (T1'First) /= Fa6 (T1'First) then
         Report.Failed ("INCORRECT RESULTS - 14");
      end if;

      if B1 not in Farr1 then
         Report.Failed ("INCORRECT RESULTS - 15");
      end if;

      if Fa2 not in Farr2 then
         Report.Failed ("INCORRECT RESULTS - 16");
      end if;

      if Fa3 not in Farr3 then
         Report.Failed ("INCORRECT RESULTS - 17");
      end if;

      if B4 not in Farr4 then
         Report.Failed ("INCORRECT RESULTS - 18");
      end if;

      if B5 not in Farr5 then
         Report.Failed ("INCORRECT RESULTS - 19");
      end if;

      if Fa6 not in Farr6 then
         Report.Failed ("INCORRECT RESULTS - 20");
      end if;

      if Fa1'Length /= Fa1'Last - Fa1'First + 1 then
         Report.Failed ("INCORRECT RESULTS - 27");
      end if;

      if Fa2'Length /= Fa2'Last - Fa2'First + 1 then
         Report.Failed ("INCORRECT RESULTS - 28");
      end if;

      if Fa3'Length /= Fa3'Last - Fa3'First + 1 then
         Report.Failed ("INCORRECT RESULTS - 29");
      end if;

      if Fa4'Length /= Fa4'Last - Fa4'First + 1 then
         Report.Failed ("INCORRECT RESULTS - 30");
      end if;

      if Fa4'Length (2) /= Fa4'Last (2) - Fa4'First (2) + 1 then
         Report.Failed ("INCORRECT RESULTS - 31");
      end if;

      if Fa5'Length /= Fa5'Last - Fa5'First + 1 then
         Report.Failed ("INCORRECT RESULTS - 32");
      end if;

      if Fa5'Length (2) /= Fa5'Last (2) - Fa5'First (2) + 1 then
         Report.Failed ("INCORRECT RESULTS - 33");
      end if;

      if Fa6'Length /= T1'Pos (Fa6'Last) - T1'Pos (Fa6'First) + 1 then
         Report.Failed ("INCORRECT RESULTS - 34");
      end if;

   end P;

   procedure Test_Procedure
     (First : in Unconstrained_Array; Ffifs : in First_Index;
      Ffils : in First_Index; Fsifs : in Second_Index; Fsils : in Second_Index;
      Fflen : in Natural; Fslen : in Natural; Ffirt : in First_Index;
      Fsirt : in Second_Index; Second : in Unconstrained_Array;
      Sfifs : in First_Index; Sfils : in First_Index; Ssifs : in Second_Index;
      Ssils : in Second_Index; Sflen : in Natural; Sslen : in Natural;
      Sfirt : in First_Index; Ssirt : in Second_Index; Remarks : in String)
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
         Report.Failed ("INCORRECT HANDLING OF 'RANGE ATTRIBUE. " & Remarks);
      end if;

   end Test_Procedure;

   procedure Ctest_Procedure
     (First : in Constrained_Array; Ffirt : in First_Index;
      Fsirt : in Second_Index; Second : in Constrained_Array;
      Sfirt : in First_Index; Ssirt : in Second_Index; Remarks : in String)
   is

   begin -- CTEST_PROCEDURE

      if (First'First /= First_Index'First) or
        (First'First (1) /= First_Index'First) or
        (First'First (2) /= Second_Index'First) or
        (Second'First /= First_Index'First) or
        (Second'First (1) /= First_Index'First) or
        (Second'First (2) /= Second_Index'First) then
         Report.Failed ("PROBLEMS WITH 'FIRST. " & Remarks);
      end if;

      if (First'Last /= First_Index'Last) or
        (First'Last (1) /= First_Index'Last) or
        (First'Last (2) /= Second_Index'Last) or
        (Second'Last /= First_Index'Last) or
        (Second'Last (1) /= First_Index'Last) or
        (Second'Last (2) /= Second_Index'Last) then
         Report.Failed ("PROBLEMS WITH 'LAST. " & Remarks);
      end if;

      if
        (First'Length /=
         First_Index'Pos (First_Index'Last) -
           First_Index'Pos (First_Index'First) + 1) or
        (First'Length (1) /=
         First_Index'Pos (First_Index'Last) -
           First_Index'Pos (First_Index'First) + 1) or
        (First'Length (2) /=
         Second_Index'Pos (Second_Index'Last) -
           Second_Index'Pos (Second_Index'First) + 1) or
        (Second'Length /=
         First_Index'Pos (First_Index'Last) -
           First_Index'Pos (First_Index'First) + 1) or
        (Second'Length (1) /=
         First_Index'Pos (First_Index'Last) -
           First_Index'Pos (First_Index'First) + 1) or
        (Second'Length (2) /=
         Second_Index'Pos (Second_Index'Last) -
           Second_Index'Pos (Second_Index'First) + 1)
      then
         Report.Failed ("PROBLEMS WITH 'LENGTH. " & Remarks);
      end if;

      if (Ffirt not in First'Range (1)) or (Ffirt not in First'Range) or
        (Sfirt not in Second'Range (1)) or (Sfirt not in Second'Range) or
        (Fsirt not in First'Range (2)) or (Ssirt not in Second'Range (2)) then
         Report.Failed ("INCORRECT HANDLING OF 'RANGE ATTRIBUE. " & Remarks);
      end if;

      if Constrained_Array'Size <= 0 then
         Report.Failed ("PROBLEMS WITH THE 'SIZE ATTRIBUTE. " & Remarks);
      end if;

      if First'Address = Second'Address then
         Report.Failed ("PROBLEMS WITH THE 'ADDRESS ATTRIBUTE. " & Remarks);
      end if;

   end Ctest_Procedure;

   procedure First_Test_Procedure is new Test_Procedure
     (First_Index         => Short_Range, Second_Index => Medium_Range,
      Unconstrained_Array => First_Template);

   procedure New_Ctest_Procedure is new Ctest_Procedure
     (First_Index    => Short_Range, Second_Index => Medium_Range,
      Component_Type => Date, Constrained_Array => Second_Template);

   procedure Np is new P (Subint, Date, Today, Arra, A1, Arra, A2, Arrb, A3,
      Arrc, A4, Arrd, A5, Arre, A6, Arre, A6);

begin  -- CC1224A

   Report.Test
     ("CC1224A",
      "FOR ARRAY TYPES WITH A NONLIMITED " &
      "COMPONENT TYPE (OF A FORMAL AND NONFORMAL GENERIC " &
      "TYPE), CHECK THAT THE FOLLOWING OPERATIONS " &
      "ARE IMPLICITY DECLARED AND ARE, THEREFORE, " &
      "AVAILABLE WITHIN THE GENERIC -- UNIT: " &
      "ASSIGNMENT, THE OPERATION ASSOCIATED WITH " &
      "AGGREGATE NOTATION, MEMBERSHIP TESTS, THE " &
      "OPERATION ASSOCIATED WITH INDEXED " &
      "COMPONENTS, QUALIFICATION, EXPLICIT " &
      "CONVERSION, 'SIZE, 'ADDRESS, 'FIRST, " &
      "'FIRST (N), 'LAST, 'LAST (N), 'RANGE, " &
      "'RANGE (N), 'LENGTH, 'LENGTH (N)");

   Np;

   First_Test_Procedure
     (First => First_Array, Ffifs => -10, Ffils => 10, Fsifs => 6, Fsils => 10,
      Fflen => 21, Fslen => 5, Ffirt => 0, Fsirt => 8, Second => Second_Array,
      Sfifs => 0, Sfils => 7, Ssifs => 1, Ssils => 15, Sflen => 8, Sslen => 15,
      Sfirt => 5, Ssirt => 13, Remarks => "FIRST_TEST_PROCEDURE");

   New_Ctest_Procedure
     (First => Third_Array, Ffirt => -5, Fsirt => 11, Second => Fourth_Array,
      Sfirt => 0, Ssirt => 14, Remarks => "NEW_CTEST_PROCEDURE");

   Report.Result;

end Cc1224a;
