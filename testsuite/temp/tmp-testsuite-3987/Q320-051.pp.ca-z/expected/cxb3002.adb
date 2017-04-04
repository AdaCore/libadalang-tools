-- CXB3002.A
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
--      Check that the specifications of the package Interfaces.C.Strings
--      are available for use.
--
-- TEST DESCRIPTION:
--      This test verifies that the types and subprograms specified for the
--      interface are present
--
-- APPLICABILITY CRITERIA:
--      If an implementation provides packages Interfaces.C and
--      Interfaces.C.Strings, this test must compile, execute, and
--      report "PASSED".
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      28 Feb 96   SAIC    Added applicability criteria.
--
--!

with Report;
with Interfaces.C;                                            -- N/A => ERROR
with Interfaces.C.Strings;                                    -- N/A => ERROR

procedure Cxb3002 is
   package Strings renames Interfaces.C.Strings;
   package C renames Interfaces.C;

begin

   Report.Test ("CXB3002", "Check the specification of Interfaces.C.Strings");

   declare  -- encapsulate the test

      Tc_Int_1      : Integer  := 1;
      Tc_Int_2      : Integer  := 1;
      Tc_String     : String   := "ABCD";
      Tc_Boolean    : Boolean  := True;
      Tc_Char_Array : C.Char_Array (1 .. 5);
      Tc_Size_T     : C.Size_T := C.Size_T'First;

      --  Note In all of the following the Strings spec. being tested
      --  is shown in comment lines
      --
      --    type char_array_access is access all char_array;
      Tst_Char_Array_Access : Strings.Char_Array_Access :=
        new Interfaces.C.Char_Array (1 .. 5);

      --    type chars_ptr is private;
      --    Null_Ptr : constant chars_ptr;
      Tst_Chars_Ptr : Strings.Chars_Ptr := Strings.Null_Ptr;

      --  type chars_ptr_array is array (size_t range <>) of chars_ptr;
      Tst_Chars_Ptr_Array : Strings.Chars_Ptr_Array (1 .. 5);

   begin    -- encapsulation

      -- Arrange that the calls to the subprograms are compiled but
      -- not executed
      --
      if not Report.Equal (Tc_Int_1, Tc_Int_2) then

         --    function To_Chars_Ptr (Item      : in char_array_access;
         --                           Nul_Check : in Boolean := False)
         --       return chars_ptr;
         Tst_Chars_Ptr :=
           Strings.To_Chars_Ptr (Tst_Char_Array_Access, Tc_Boolean);

         --    This one is out of LRM order so that we can "initialize"
         --    TC_char_array for the "in" parameter of the next one
         --
         --    function Value (Item : in chars_ptr) return char_array;
         Tc_Char_Array := Strings.Value (Tst_Chars_Ptr);

         --    function New_Char_Array (Chars   : in char_array)
         --       return chars_ptr;
         Tst_Chars_Ptr := Strings.New_Char_Array (Tc_Char_Array);

         --    function New_String (Str : in String) return chars_ptr;
         Tst_Chars_Ptr := Strings.New_String ("TEST STRING");

         --    procedure Free (Item : in out chars_ptr);
         Strings.Free (Tst_Chars_Ptr);

         --    function Value (Item : in chars_ptr; Length : in size_t)
         --       return char_array;
         Tc_Char_Array := Strings.Value (Tst_Chars_Ptr, Tc_Size_T);

         -- Use Report.Comment as a known procedure which takes a string as
         -- a parameter (this does not actually get output)
         --    function Value (Item : in chars_ptr) return String;
         Report.Comment (Strings.Value (Tst_Chars_Ptr));

         --    function Value (Item : in chars_ptr; Length : in size_t)
         --       return String;
         Tc_String := Strings.Value (Tst_Chars_Ptr, Tc_Size_T);

         --    function Strlen (Item : in chars_ptr) return size_t;
         Tc_Size_T := Strings.Strlen (Tst_Chars_Ptr);

         --    procedure Update (Item   : in chars_ptr;
         --                      Offset : in size_t;
         --                      Chars  : in char_array;
         --                      Check  : in Boolean := True);
         Strings.Update (Tst_Chars_Ptr, Tc_Size_T, Tc_Char_Array, Tc_Boolean);

         --    procedure Update (Item   : in chars_ptr;
         --                      Offset : in size_t;
         --                      Str    : in String;
         --                      Check  : in Boolean := True);
         Strings.Update (Tst_Chars_Ptr, Tc_Size_T, Tc_String, Tc_Boolean);

         --    Update_Error : exception;
         raise Strings.Update_Error;

      end if;

      if not Report.Equal (Tc_Int_2, Tc_Int_1) then

         -- This exception is out of LRM presentation order to avoid
         -- compiler warnings about unreachable code
         --    Dereference_Error : exception;
         raise Strings.Dereference_Error;

      end if;

   end;     -- encapsulation

   Report.Result;

end Cxb3002;
