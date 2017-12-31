-- CXB3001.A
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
--      Check that the specifications of the package Interfaces.C are
--      available for use.
--
-- TEST DESCRIPTION:
--      This test verifies that the types and subprograms specified for the
--      interface are present.  It just checks for the presence of
--      the subprograms.  Other tests are designed to exercise the interface.
--
-- APPLICABILITY CRITERIA:
--      If an implementation provides package Interfaces.C, this test
--      must compile, execute, and report "PASSED".
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      15 Nov 95   SAIC    Corrected To_C parameter list for ACVC 2.0.1.
--      28 Feb 96   SAIC    Added applicability criteria.
--
--!

with Report;
with Interfaces.C;                                             -- N/A => ERROR

procedure Cxb3001 is
   package C renames Interfaces.C;
   use type C.Signed_Char;
   use type C.Unsigned_Char;
   use type C.Char;

begin

   Report.Test ("CXB3001", "Check the specification of Interfaces.C");

   declare  -- encapsulate the test

      Tst_Char_Bit  : constant := C.Char_Bit;
      Tst_Schar_Min : constant := C.Schar_Min;
      Tst_Schar_Max : constant := C.Schar_Max;
      Tst_Uchar_Max : constant := C.Uchar_Max;

      -- Signed and Unsigned Integers

      Tst_Int   : C.Int   := C.Int'First;
      Tst_Short : C.Short := C.Short'First;
      Tst_Long  : C.Long  := C.Long'First;

      Tst_Signed_Char_Min : C.Signed_Char := C.Signed_Char'First;
      Tst_Signed_Char_Max : C.Signed_Char := C.Signed_Char'Last;

      Tst_Unsigned       : C.Unsigned;
      Tst_Unsigned_Short : C.Unsigned_Short;
      Tst_Unsigned_Long  : C.Unsigned_Long;

      Tst_Unsigned_Char : C.Unsigned_Char;
      Tst_Plain_Char    : C.Plain_Char;

      Tst_Ptrdiff_T : C.Ptrdiff_T;
      Tst_Size_T    : C.Size_T;

      -- Floating-Point

      Tst_C_Float     : C.C_Float;
      Tst_Double      : C.Double;
      Tst_Long_Double : C.Long_Double;

      -- Characters and Strings

      Tst_Char : C.Char;
      Tst_Nul  : C.Char := C.Nul;

      -- Collect all the subprogram calls such that they are compiled but not
      -- executed
      --
      procedure Collect_All_Calls is

         Cac_Char           : C.Char;
         Cac_Character      : Character;
         Cac_String         : String (1 .. 5);
         Cac_Boolean        : Boolean := False;
         Cac_Char_Array     : C.Char_Array (1 .. 5);
         Cac_Integer        : Integer;
         Cac_Natural        : Natural;
         Cac_Wchar_T        : C.Wchar_T;
         Cac_Wide_Character : Wide_Character;
         Cac_Wchar_Array    : C.Wchar_Array (1 .. 5);
         Cac_Wide_String    : Wide_String (1 .. 5);
         Cac_Size_T         : C.Size_T;

      begin

         Cac_Char      := C.To_C (Cac_Character);
         Cac_Character := C.To_Ada (Cac_Char);

         Cac_Char_Array := C.To_C (Cac_String, Cac_Boolean);
         Cac_String     := C.To_Ada (Cac_Char_Array, Cac_Boolean);

         -- This call is out of LRM order so that we can use the array
         -- initialized above
         Cac_Boolean := C.Is_Nul_Terminated (Cac_Char_Array);

         C.To_C (Cac_String, Cac_Char_Array, Cac_Size_T, Cac_Boolean);
         C.To_Ada (Cac_Char_Array, Cac_String, Cac_Natural, Cac_Boolean);

         Cac_Wchar_T        := C.To_C (Cac_Wide_Character);
         Cac_Wide_Character := C.To_Ada (Cac_Wchar_T);
         Cac_Wchar_T        := C.Wide_Nul;

         Cac_Wchar_Array := C.To_C (Cac_Wide_String, Cac_Boolean);
         Cac_Wide_String := C.To_Ada (Cac_Wchar_Array, Cac_Boolean);

         -- This call is out of LRM order so that we can use the array
         -- initialized above
         Cac_Boolean := C.Is_Nul_Terminated (Cac_Wchar_Array);

         C.To_C (Cac_Wide_String, Cac_Wchar_Array, Cac_Size_T, Cac_Boolean);
         C.To_Ada (Cac_Wchar_Array, Cac_Wide_String, Cac_Natural, Cac_Boolean);

         raise C.Terminator_Error;

      end Collect_All_Calls;

   begin    -- encapsulation

      if Tst_Signed_Char_Min /= C.Schar_Min then
         Report.Failed ("tst_signed_char_min is incorrect");
      end if;
      if Tst_Signed_Char_Max /= C.Schar_Max then
         Report.Failed ("tst_signed_char_max is incorrect");
      end if;
      if C.Signed_Char'Size /= C.Char_Bit then
         Report.Failed ("C.signed_char'Size is incorrect");
      end if;

      if C.Unsigned_Char'First /= 0 or C.Unsigned_Char'Last /= C.Uchar_Max or
        C.Unsigned_Char'Size /= C.Char_Bit then

         Report.Failed ("unsigned_char is incorrectly defined");

      end if;

      if Tst_Nul /= C.Char'First then
         Report.Failed ("tst_nul is incorrect");
      end if;

   end;     -- encapsulation

   Report.Result;

end Cxb3001;
