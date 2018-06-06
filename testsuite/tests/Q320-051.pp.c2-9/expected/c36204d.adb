-- C36204D.ADA

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
-- CHECK THAT EACH ARRAY ATTRIBUTE YIELDS THE CORRECT VALUES. BOTH ARRAY
-- OBJECTS AND TYPES ARE CHECKED. THIS TEST CHECKS THE ABOVE FOR ARRAYS
-- WITHIN GENERIC PROGRAM UNITS.

-- HISTROY
--  EDWARD V. BERARD, 9 AUGUST 1990

with Report;
with System;

procedure C36204d is

   Short_Start : constant := -10;
   Short_End   : constant := 10;
   type Short_Range is range Short_Start .. Short_End;
   Short_Length : constant Natural := (Short_End - Short_Start + 1);

   type Month_Type is
     (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
   subtype Mid_Year is Month_Type range May .. Aug;
   type Day_Type is range 1 .. 31;
   type Year_Type is range 1_904 .. 2_050;
   type Date is record
      Month : Month_Type;
      Day   : Day_Type;
      Year  : Year_Type;
   end record;

   Today : Date := (Month => Aug, Day => 10, Year => 1_990);

   First_Date : Date := (Day => 6, Month => Jun, Year => 1_967);

   function "="
     (Left : in System.Address; Right : in System.Address)
      return Boolean renames
     System."=";

   generic

      type First_Index is (<>);
      First_Index_Length : in Natural;
      First_Test_Value : in First_Index;
      type Second_Index is (<>);
      Second_Index_Length : in Natural;
      Second_Test_Value : in Second_Index;
      type Third_Index is (<>);
      Third_Index_Length : in Natural;
      Third_Test_Value : in Third_Index;
      type First_Component_Type is private;
      First_Default_Value : in First_Component_Type;
      Second_Default_Value : in First_Component_Type;
      type Second_Component_Type is private;
      Third_Default_Value : in Second_Component_Type;
      Fourth_Default_Value : in Second_Component_Type;

   package Array_Attribute_Test is

      type Matrix is array (First_Index, Second_Index) of First_Component_Type;

      type Cube is
        array
          (First_Index, Second_Index, Third_Index) of Second_Component_Type;

   end Array_Attribute_Test;

   package body Array_Attribute_Test is

      First_Array : Matrix :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last => First_Default_Value));

      Second_Array : Cube :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last =>
              (Third_Index'First .. Third_Index'Last => Third_Default_Value)));

      Third_Array : constant Matrix :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last => Second_Default_Value));

      Fourth_Array : constant Cube :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last =>
              (Third_Index'First .. Third_Index'Last =>
                 Fourth_Default_Value)));

      Fa1 : First_Index  := First_Array'First (1);
      Fa2 : First_Index  := First_Array'Last (1);
      Fa3 : Second_Index := First_Array'First (2);
      Fa4 : Second_Index := First_Array'Last (2);

      Sa1 : First_Index  := Second_Array'First (1);
      Sa2 : First_Index  := Second_Array'Last (1);
      Sa3 : Second_Index := Second_Array'First (2);
      Sa4 : Second_Index := Second_Array'Last (2);
      Sa5 : Third_Index  := Second_Array'First (3);
      Sa6 : Third_Index  := Second_Array'Last (3);

      Fal1 : Natural := First_Array'Length (1);
      Fal2 : Natural := First_Array'Length (2);

      Sal1 : Natural := Second_Array'Length (1);
      Sal2 : Natural := Second_Array'Length (2);
      Sal3 : Natural := Second_Array'Length (3);

      Matrix_Size : Natural := Matrix'Size;
      Cube_Size   : Natural := Cube'Size;

      Faa  : System.Address := First_Array'Address;
      Saa  : System.Address := Second_Array'Address;
      Taa  : System.Address := Third_Array'Address;
      Fraa : System.Address := Fourth_Array'Address;

   begin  -- ARRAY_ATTRIBUTE_TEST

      if (Fa1 /= First_Index'First) or (Fa3 /= Second_Index'First) or
        (Sa1 /= First_Index'First) or (Sa3 /= Second_Index'First) or
        (Sa5 /= Third_Index'First) then
         Report.Failed ("INCORRECT VALUE RETURNED FOR 'FIRST - PACKAGE");
      end if;

      if (Fa2 /= First_Index'Last) or (Fa4 /= Second_Index'Last) or
        (Sa2 /= First_Index'Last) or (Sa4 /= Second_Index'Last) or
        (Sa6 /= Third_Index'Last) then
         Report.Failed ("INCORRECT VALUE RETURNED FOR 'LAST - PACKAGE");
      end if;

      if (Fal1 /= First_Index_Length) or (Fal2 /= Second_Index_Length) or
        (Sal1 /= First_Index_Length) or (Sal2 /= Second_Index_Length) or
        (Sal3 /= Third_Index_Length) then
         Report.Failed ("INCORRECT VALUE RETURNED FOR 'LENGTH - PACKAGE");
      end if;

      for Outer_Index in First_Array'Range (1) loop
         for Inner_Index in First_Array'Range (2) loop
            First_Array (Outer_Index, Inner_Index) := Second_Default_Value;
         end loop;
      end loop;

      if First_Array /= Third_Array then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
            "FOR 2-DIMENSIONAL ARRAY. - PACKAGE");
      end if;

      for Outer_Index in Second_Array'Range (1) loop
         for Middle_Index in Second_Array'Range (2) loop
            for Inner_Index in Second_Array'Range (3) loop
               Second_Array (Outer_Index, Middle_Index, Inner_Index) :=
                 Fourth_Default_Value;
            end loop;
         end loop;
      end loop;

      if Second_Array /= Fourth_Array then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
            "FOR 3-DIMENSIONAL ARRAY. - PACKAGE");
      end if;

      if (First_Test_Value not in First_Array'Range (1)) or
        (First_Test_Value not in Second_Array'Range (1)) or
        (Second_Test_Value not in First_Array'Range (2)) or
        (Second_Test_Value not in Second_Array'Range (2)) or
        (Third_Test_Value not in Second_Array'Range (3)) then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " & "- PACKAGE");
      end if;

      if (Matrix_Size = 0) or (Cube_Size = 0) then
         Report.Failed
           ("INCORRECT HANDLING OF THE 'SIZE ATTRIBUTE. " & "- PACKAGE");
      end if;

      if (Faa = Taa) or (Saa = Fraa) or (Faa = Saa) or (Faa = Fraa) or
        (Saa = Taa) or (Taa = Fraa) then
         Report.Failed
           ("INCORRECT HANDLING OF THE 'ADDRESS ATTRIBUTE. " & "- PACKAGE");
      end if;

   end Array_Attribute_Test;

   generic

      type First_Index is (<>);
      First_Index_Length : in Natural;
      First_Test_Value : in First_Index;
      type Second_Index is (<>);
      Second_Index_Length : in Natural;
      Second_Test_Value : in Second_Index;
      type Third_Index is (<>);
      Third_Index_Length : in Natural;
      Third_Test_Value : in Third_Index;
      type First_Component_Type is private;
      First_Default_Value : in First_Component_Type;
      Second_Default_Value : in First_Component_Type;
      type Second_Component_Type is private;
      Third_Default_Value : in Second_Component_Type;
      Fourth_Default_Value : in Second_Component_Type;

   procedure Proc_Array_Att_Test;

   procedure Proc_Array_Att_Test is

      type Matrix is array (First_Index, Second_Index) of First_Component_Type;

      type Cube is
        array
          (First_Index, Second_Index, Third_Index) of Second_Component_Type;

      First_Array : Matrix :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last => First_Default_Value));

      Second_Array : Cube :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last =>
              (Third_Index'First .. Third_Index'Last => Third_Default_Value)));

      Third_Array : constant Matrix :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last => Second_Default_Value));

      Fourth_Array : constant Cube :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last =>
              (Third_Index'First .. Third_Index'Last =>
                 Fourth_Default_Value)));

      Fa1 : First_Index  := First_Array'First (1);
      Fa2 : First_Index  := First_Array'Last (1);
      Fa3 : Second_Index := First_Array'First (2);
      Fa4 : Second_Index := First_Array'Last (2);

      Sa1 : First_Index  := Second_Array'First (1);
      Sa2 : First_Index  := Second_Array'Last (1);
      Sa3 : Second_Index := Second_Array'First (2);
      Sa4 : Second_Index := Second_Array'Last (2);
      Sa5 : Third_Index  := Second_Array'First (3);
      Sa6 : Third_Index  := Second_Array'Last (3);

      Fal1 : Natural := First_Array'Length (1);
      Fal2 : Natural := First_Array'Length (2);

      Sal1 : Natural := Second_Array'Length (1);
      Sal2 : Natural := Second_Array'Length (2);
      Sal3 : Natural := Second_Array'Length (3);

      Matrix_Size : Natural := Matrix'Size;
      Cube_Size   : Natural := Cube'Size;

      Faa  : System.Address := First_Array'Address;
      Saa  : System.Address := Second_Array'Address;
      Taa  : System.Address := Third_Array'Address;
      Fraa : System.Address := Fourth_Array'Address;

   begin  -- PROC_ARRAY_ATT_TEST

      if (Fa1 /= First_Index'First) or (Fa3 /= Second_Index'First) or
        (Sa1 /= First_Index'First) or (Sa3 /= Second_Index'First) or
        (Sa5 /= Third_Index'First) then
         Report.Failed
           ("INCORRECT VALUE RETURNED FOR 'FIRST " & "- PROCEDURE");
      end if;

      if (Fa2 /= First_Index'Last) or (Fa4 /= Second_Index'Last) or
        (Sa2 /= First_Index'Last) or (Sa4 /= Second_Index'Last) or
        (Sa6 /= Third_Index'Last) then
         Report.Failed ("INCORRECT VALUE RETURNED FOR 'LAST " & "- PROCEDURE");
      end if;

      if (Fal1 /= First_Index_Length) or (Fal2 /= Second_Index_Length) or
        (Sal1 /= First_Index_Length) or (Sal2 /= Second_Index_Length) or
        (Sal3 /= Third_Index_Length) then
         Report.Failed
           ("INCORRECT VALUE RETURNED FOR 'LENGTH " & "- PROCEDURE");
      end if;

      for Outer_Index in First_Array'Range (1) loop
         for Inner_Index in First_Array'Range (2) loop
            First_Array (Outer_Index, Inner_Index) := Second_Default_Value;
         end loop;
      end loop;

      if First_Array /= Third_Array then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
            "FOR 2-DIMENSIONAL ARRAY. - PROCEDURE");
      end if;

      for Outer_Index in Second_Array'Range (1) loop
         for Middle_Index in Second_Array'Range (2) loop
            for Inner_Index in Second_Array'Range (3) loop
               Second_Array (Outer_Index, Middle_Index, Inner_Index) :=
                 Fourth_Default_Value;
            end loop;
         end loop;
      end loop;

      if Second_Array /= Fourth_Array then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
            "FOR 3-DIMENSIONAL ARRAY. - PROCEDURE");
      end if;

      if (First_Test_Value not in First_Array'Range (1)) or
        (First_Test_Value not in Second_Array'Range (1)) or
        (Second_Test_Value not in First_Array'Range (2)) or
        (Second_Test_Value not in Second_Array'Range (2)) or
        (Third_Test_Value not in Second_Array'Range (3)) then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " & "- PROCEDURE");
      end if;

      if (Matrix_Size = 0) or (Cube_Size = 0) then
         Report.Failed
           ("INCORRECT HANDLING OF THE 'SIZE ATTRIBUTE. " & "- PROCEDURE");
      end if;

      if (Faa = Taa) or (Saa = Fraa) or (Faa = Saa) or (Faa = Fraa) or
        (Saa = Taa) or (Taa = Fraa) then
         Report.Failed
           ("INCORRECT HANDLING OF THE 'ADDRESS ATTRIBUTE. " & "- PROCEDURE");
      end if;

   end Proc_Array_Att_Test;

   generic

      type First_Index is (<>);
      First_Index_Length : in Natural;
      First_Test_Value : in First_Index;
      type Second_Index is (<>);
      Second_Index_Length : in Natural;
      Second_Test_Value : in Second_Index;
      type Third_Index is (<>);
      Third_Index_Length : in Natural;
      Third_Test_Value : in Third_Index;
      type First_Component_Type is private;
      First_Default_Value : in First_Component_Type;
      Second_Default_Value : in First_Component_Type;
      type Second_Component_Type is private;
      Third_Default_Value : in Second_Component_Type;
      Fourth_Default_Value : in Second_Component_Type;

   function Func_Array_Att_Test return Boolean;

   function Func_Array_Att_Test return Boolean is

      type Matrix is array (First_Index, Second_Index) of First_Component_Type;

      type Cube is
        array
          (First_Index, Second_Index, Third_Index) of Second_Component_Type;

      First_Array : Matrix :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last => First_Default_Value));

      Second_Array : Cube :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last =>
              (Third_Index'First .. Third_Index'Last => Third_Default_Value)));

      Third_Array : constant Matrix :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last => Second_Default_Value));

      Fourth_Array : constant Cube :=
        (First_Index'First .. First_Index'Last =>
           (Second_Index'First .. Second_Index'Last =>
              (Third_Index'First .. Third_Index'Last =>
                 Fourth_Default_Value)));

      Fa1 : First_Index  := First_Array'First (1);
      Fa2 : First_Index  := First_Array'Last (1);
      Fa3 : Second_Index := First_Array'First (2);
      Fa4 : Second_Index := First_Array'Last (2);

      Sa1 : First_Index  := Second_Array'First (1);
      Sa2 : First_Index  := Second_Array'Last (1);
      Sa3 : Second_Index := Second_Array'First (2);
      Sa4 : Second_Index := Second_Array'Last (2);
      Sa5 : Third_Index  := Second_Array'First (3);
      Sa6 : Third_Index  := Second_Array'Last (3);

      Fal1 : Natural := First_Array'Length (1);
      Fal2 : Natural := First_Array'Length (2);

      Sal1 : Natural := Second_Array'Length (1);
      Sal2 : Natural := Second_Array'Length (2);
      Sal3 : Natural := Second_Array'Length (3);

      Matrix_Size : Natural := Matrix'Size;
      Cube_Size   : Natural := Cube'Size;

      Faa  : System.Address := First_Array'Address;
      Saa  : System.Address := Second_Array'Address;
      Taa  : System.Address := Third_Array'Address;
      Fraa : System.Address := Fourth_Array'Address;

   begin  -- FUNC_ARRAY_ATT_TEST

      if (Fa1 /= First_Index'First) or (Fa3 /= Second_Index'First) or
        (Sa1 /= First_Index'First) or (Sa3 /= Second_Index'First) or
        (Sa5 /= Third_Index'First) then
         Report.Failed ("INCORRECT VALUE RETURNED FOR 'FIRST " & "- FUNCTION");
      end if;

      if (Fa2 /= First_Index'Last) or (Fa4 /= Second_Index'Last) or
        (Sa2 /= First_Index'Last) or (Sa4 /= Second_Index'Last) or
        (Sa6 /= Third_Index'Last) then
         Report.Failed ("INCORRECT VALUE RETURNED FOR 'LAST " & "- FUNCTION");
      end if;

      if (Fal1 /= First_Index_Length) or (Fal2 /= Second_Index_Length) or
        (Sal1 /= First_Index_Length) or (Sal2 /= Second_Index_Length) or
        (Sal3 /= Third_Index_Length) then
         Report.Failed
           ("INCORRECT VALUE RETURNED FOR 'LENGTH " & "- FUNCTION");
      end if;

      for Outer_Index in First_Array'Range (1) loop
         for Inner_Index in First_Array'Range (2) loop
            First_Array (Outer_Index, Inner_Index) := Second_Default_Value;
         end loop;
      end loop;

      if First_Array /= Third_Array then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
            "FOR 2-DIMENSIONAL ARRAY. - FUNCTION");
      end if;

      for Outer_Index in Second_Array'Range (1) loop
         for Middle_Index in Second_Array'Range (2) loop
            for Inner_Index in Second_Array'Range (3) loop
               Second_Array (Outer_Index, Middle_Index, Inner_Index) :=
                 Fourth_Default_Value;
            end loop;
         end loop;
      end loop;

      if Second_Array /= Fourth_Array then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
            "FOR 3-DIMENSIONAL ARRAY. - FUNCTION");
      end if;

      if (First_Test_Value not in First_Array'Range (1)) or
        (First_Test_Value not in Second_Array'Range (1)) or
        (Second_Test_Value not in First_Array'Range (2)) or
        (Second_Test_Value not in Second_Array'Range (2)) or
        (Third_Test_Value not in Second_Array'Range (3)) then
         Report.Failed
           ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " & "- FUNCTION");
      end if;

      if (Matrix_Size = 0) or (Cube_Size = 0) then
         Report.Failed
           ("INCORRECT HANDLING OF THE 'SIZE ATTRIBUTE. " & "- FUNCTION");
      end if;

      if (Faa = Taa) or (Saa = Fraa) or (Faa = Saa) or (Faa = Fraa) or
        (Saa = Taa) or (Taa = Fraa) then
         Report.Failed
           ("INCORRECT HANDLING OF THE 'ADDRESS ATTRIBUTE. " & "- FUNCTION");
      end if;

      return True;

   end Func_Array_Att_Test;

begin -- C36204D

   Report.Test
     ("C36204D",
      "ARRAY ATTRIBUTES RETURN CORRECT " &
      "VALUES WITHIN GENERIC PROGRAM UNITS.");

   Local_Block :

   declare

      Dummy : Boolean := False;

      package New_Array_Attribute_Test is new Array_Attribute_Test
        (First_Index => Short_Range, First_Index_Length => Short_Length,
         First_Test_Value      => -7, Second_Index => Month_Type,
         Second_Index_Length   => 12, Second_Test_Value => Aug,
         Third_Index           => Boolean, Third_Index_Length => 2,
         Third_Test_Value      => False, First_Component_Type => Month_Type,
         First_Default_Value   => Jan, Second_Default_Value => Dec,
         Second_Component_Type => Date, Third_Default_Value => Today,
         Fourth_Default_Value  => First_Date);

      procedure New_Proc_Array_Att_Test is new Proc_Array_Att_Test
        (First_Index           => Month_Type, First_Index_Length => 12,
         First_Test_Value      => Aug, Second_Index => Short_Range,
         Second_Index_Length   => Short_Length, Second_Test_Value => -7,
         Third_Index           => Boolean, Third_Index_Length => 2,
         Third_Test_Value      => False, First_Component_Type => Date,
         First_Default_Value   => Today, Second_Default_Value => First_Date,
         Second_Component_Type => Month_Type, Third_Default_Value => Jan,
         Fourth_Default_Value  => Dec);

      function New_Func_Array_Att_Test is new Func_Array_Att_Test
        (First_Index           => Day_Type, First_Index_Length => 31,
         First_Test_Value      => 25, Second_Index => Short_Range,
         Second_Index_Length   => Short_Length, Second_Test_Value => -7,
         Third_Index           => Mid_Year, Third_Index_Length => 4,
         Third_Test_Value      => Jul, First_Component_Type => Date,
         First_Default_Value   => Today, Second_Default_Value => First_Date,
         Second_Component_Type => Month_Type, Third_Default_Value => Jan,
         Fourth_Default_Value  => Dec);

   begin  -- LOCAL_BLOCK

      New_Proc_Array_Att_Test;

      Dummy := New_Func_Array_Att_Test;
      if not Dummy then
         Report.Failed ("WRONG VALUE RETURNED BY FUNCTION.");
      end if;

   end Local_Block;

   Report.Result;

end C36204d;
