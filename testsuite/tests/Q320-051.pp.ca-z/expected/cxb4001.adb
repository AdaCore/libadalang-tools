-- CXB4001.A
--
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
--
-- OBJECTIVE:
--      Check that the specifications of the package Interfaces.COBOL
--      are available for use
--
-- TEST DESCRIPTION:
--      This test verifies that the type and the subprograms specified for
--      the interface are present.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and
--      report "PASSED".
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      15 Nov 95   SAIC    Corrected visibility errors for ACVC 2.0.1.
--      28 Feb 96   SAIC    Added applicability criteria.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--      01 DEC 97   EDS     Change "To_Comp" to "To_Binary".
--!

with Report;
with Interfaces.Cobol;                                        -- N/A => ERROR

procedure Cxb4001 is

   package Cobol renames Interfaces.Cobol;
   use type Cobol.Byte;
   use type Cobol.Decimal_Element;

begin

   Report.Test ("CXB4001", "Check the specification of Interfaces.COBOL");

   declare  -- encapsulate the test

      --  Types and operations for internal data representations

      Tst_Floating      : Cobol.Floating;
      Tst_Long_Floating : Cobol.Long_Floating;

      Tst_Binary      : Cobol.Binary;
      Tst_Long_Binary : Cobol.Long_Binary;

      Tst_Max_Digits_Binary      : constant := Cobol.Max_Digits_Binary;
      Tst_Max_Digits_Long_Binary : constant := Cobol.Max_Digits_Long_Binary;

      Tst_Decimal_Element : Cobol.Decimal_Element;

      Tst_Packed_Decimal : Cobol.Packed_Decimal (1 .. 5) :=
        (others => Cobol.Decimal_Element'First);

      --  initialize it so it can reasonably be used later
      Tst_Cobol_Character : Cobol.Cobol_Character :=
        Cobol.Cobol_Character'First;

      Tst_Ada_To_Cobol : Cobol.Cobol_Character :=
        Cobol.Ada_To_Cobol (Character'First);

      Tst_Cobol_To_Ada : Character :=
        Cobol.Cobol_To_Ada (Cobol.Cobol_Character'First);

      --  assignment to make sure it is an array of COBOL_Character
      Tst_Alphanumeric : Cobol.Alphanumeric (1 .. 5) :=
        (others => Tst_Cobol_Character);

      --  assignment to make sure it is an array of COBOL_Character
      Tst_Numeric : Cobol.Numeric (1 .. 5) := (others => Tst_Cobol_Character);

      procedure Collect_All_Calls is

         Cac_Alphanumeric : Cobol.Alphanumeric (1 .. 5) :=
           Cobol.To_Cobol ("abcde");
         Cac_String  : String (1 .. 5) := "vwxyz";
         Cac_Natural : Natural         := 0;

      begin

         Cac_Alphanumeric := Cobol.To_Cobol (Cac_String);
         Cac_String       := Cobol.To_Ada (Cac_Alphanumeric);

         Cobol.To_Cobol (Cac_String, Cac_Alphanumeric, Cac_Natural);
         Cobol.To_Ada (Cac_Alphanumeric, Cac_String, Cac_Natural);

         raise Cobol.Conversion_Error;

      end Collect_All_Calls;

      --  Formats for COBOL data representations

      Tst_Unsigned            : Cobol.Display_Format := Cobol.Unsigned;
      Tst_Leading_Separate    : Cobol.Display_Format := Cobol.Leading_Separate;
      Tst_Trailing_Separate : Cobol.Display_Format := Cobol.Trailing_Separate;
      Tst_Leading_Nonseparate : Cobol.Display_Format :=
        Cobol.Leading_Nonseparate;
      Tst_Trailing_Nonseparate : Cobol.Display_Format :=
        Cobol.Trailing_Nonseparate;

      Tst_High_Order_First : Cobol.Binary_Format := Cobol.High_Order_First;
      Tst_Low_Order_First  : Cobol.Binary_Format := Cobol.Low_Order_First;
      Tst_Native_Binary    : Cobol.Binary_Format := Cobol.Native_Binary;

      Tst_Packed_Unsigned : Cobol.Packed_Format := Cobol.Packed_Unsigned;
      Tst_Packed_Signed   : Cobol.Packed_Format := Cobol.Packed_Signed;

      --  Types for external representation of COBOL binary data

      Tst_Byte_Array : Cobol.Byte_Array (1 .. 5) :=
        (others => Cobol.Byte'First);

      -- Now instantiate one version of the generic
      --
      type Bx4001_Decimal is delta 0.1 digits 5;
      package Bx4001_Conv is new Cobol.Decimal_Conversions (Bx4001_Decimal);

      procedure Collect_All_Generic_Calls is
         Cagc_Natural        : Natural;
         Cagc_Display_Format : Cobol.Display_Format;
         Cagc_Boolean        : Boolean;
         Cagc_Numeric        : Cobol.Numeric (1 .. 5);
         Cagc_Num            : Bx4001_Decimal;
         Cagc_Packed_Decimal : Cobol.Packed_Decimal (1 .. 5);
         Cagc_Packed_Format  : Cobol.Packed_Format;
         Cagc_Byte_Array     : Cobol.Byte_Array (1 .. 5);
         Cagc_Binary_Format  : Cobol.Binary_Format;
         Cagc_Binary         : Cobol.Binary;
         Cagc_Long_Binary    : Cobol.Long_Binary;
      begin

         --  Display Formats: data values are represented as Numeric

         Cagc_Boolean := Bx4001_Conv.Valid (Cagc_Numeric, Cagc_Display_Format);
         Cagc_Natural := Bx4001_Conv.Length (Cagc_Display_Format);

         Cagc_Num :=
           Bx4001_Conv.To_Decimal (Cagc_Numeric, Cagc_Display_Format);
         Cagc_Numeric :=
           Bx4001_Conv.To_Display (Cagc_Num, Cagc_Display_Format);

         --  Packed Formats: data values are represented as Packed_Decimal

         Cagc_Boolean :=
           Bx4001_Conv.Valid (Cagc_Packed_Decimal, Cagc_Packed_Format);

         Cagc_Natural := Bx4001_Conv.Length (Cagc_Packed_Format);

         Cagc_Num :=
           Bx4001_Conv.To_Decimal (Cagc_Packed_Decimal, Cagc_Packed_Format);

         Cagc_Packed_Decimal :=
           Bx4001_Conv.To_Packed (Cagc_Num, Cagc_Packed_Format);

         --  Binary Formats: external data values are represented as Byte_Array

         Cagc_Boolean :=
           Bx4001_Conv.Valid (Cagc_Byte_Array, Cagc_Binary_Format);

         Cagc_Natural := Bx4001_Conv.Length (Cagc_Binary_Format);
         Cagc_Num     :=
           Bx4001_Conv.To_Decimal (Cagc_Byte_Array, Cagc_Binary_Format);

         Cagc_Byte_Array :=
           Bx4001_Conv.To_Binary (Cagc_Num, Cagc_Binary_Format);

         --  Internal Binary formats: data values are of type
         --  Binary/Long_Binary

         Cagc_Num := Bx4001_Conv.To_Decimal (Cagc_Binary);
         Cagc_Num := Bx4001_Conv.To_Decimal (Cagc_Long_Binary);

         Cagc_Binary      := Bx4001_Conv.To_Binary (Cagc_Num);
         Cagc_Long_Binary := Bx4001_Conv.To_Long_Binary (Cagc_Num);

      end Collect_All_Generic_Calls;

   begin    -- encapsulation

      if Cobol.Byte'First /= 0 or
        Cobol.Byte'Last /= (2**Cobol.Cobol_Character'Size) - 1 then
         Report.Failed ("Byte is incorrectly defined");
      end if;

      if Cobol.Decimal_Element'First /= 0 then
         Report.Failed ("Decimal_Element is incorrectly defined");
      end if;

   end;     -- encapsulation

   Report.Result;

end Cxb4001;
