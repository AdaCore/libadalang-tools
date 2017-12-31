-- CXB3011.A
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
--      Check that the version of Function Value with a chars_ptr parameter
--      that returns a String result returns an Ada string containing the
--      characters pointed to by the chars_ptr parameter, up to (but not
--      including) the terminating nul.
--
--      Check that the version of Function Value with a chars_ptr parameter
--      and a size_t parameter that returns a String result returns the
--      shorter of:
--        1) a String of the first size_t number of characters, or
--        2) a String of characters up to (but not including) the
--           terminating nul.
--
--      Check that the Function Strlen returns a size_t result that
--      corresponds to the number of chars in the array pointed to by Item,
--      up to but not including the terminating nul.
--
--      Check that both of the above versions of Function Value and
--      Function Strlen propagate Dereference_Error if the Item parameter
--      is Null_Ptr.
--
-- TEST DESCRIPTION:
--      This test validates two versions of Function Value, and the Function
--      Strlen.  A series of char_ptr values are provided as input, and
--      results are compared for length or content.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', 'a'..'z', 'A'..'Z', '0'..'9', '*' and '.'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.C.Strings.  If an implementation provides
--      package Interfaces.C.Strings, this test must compile, execute,
--      and report "PASSED".
--
--
-- CHANGE HISTORY:
--      28 Sep 95   SAIC    Initial prerelease version.
--      13 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Ada.Characters.Latin_1;
with Interfaces.C.Strings;                                    -- N/A => ERROR

procedure Cxb3011 is
begin

   Report.Test
     ("CXB3011",
      "Check that the two versions of Function Value " &
      "returning a String result, and the Function " &
      "Strlen, produce correct results");

   Test_Block :
   declare

      package Ic renames Interfaces.C;
      package Ics renames Interfaces.C.Strings;
      package Acl1 renames Ada.Characters.Latin_1;

      use type Ic.Char_Array;
      use type Ic.Size_T;
      use type Ics.Chars_Ptr;

      Null_Char_Array_Access : constant Ics.Char_Array_Access := null;

      Tc_String       : String (1 .. 5)          := (others => 'X');
      Tc_String_1     : constant String          := "*.3*0";
      Tc_String_2     : constant String          := "Two";
      Tc_String_3     : constant String          := "Five5";
      Tc_Blank_String : constant String (1 .. 5) := (others => ' ');

      Tc_Char_Array       : Ic.Char_Array := Ic.To_C (Tc_Blank_String, True);
      Tc_Char_Array_1 : constant Ic.Char_Array := Ic.To_C (Tc_String_1, True);
      Tc_Char_Array_2 : constant Ic.Char_Array := Ic.To_C (Tc_String_2, True);
      Tc_Char_Array_3 : constant Ic.Char_Array := Ic.To_C (Tc_String_3, True);
      Tc_Blank_Char_Array : constant Ic.Char_Array :=
        Ic.To_C (Tc_Blank_String, True);

      Tc_Chars_Ptr : Ics.Chars_Ptr := Ics.New_Char_Array (Tc_Blank_Char_Array);

      Tc_Size_T : Ic.Size_T := Ic.Size_T'First;

   begin

      -- Check that the version of Function Value with a chars_ptr parameter
      -- that returns a String result returns an Ada string containing the
      -- characters pointed to by the chars_ptr parameter, up to (but not
      -- including) the terminating nul.

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array_1);
      Tc_String    := Ics.Value (Item => Tc_Chars_Ptr);

      if Tc_String /= Tc_String_1 or Tc_String (Tc_String'Last) = Acl1.Nul then
         Report.Failed ("Incorrect result from Function Value - 1");
      end if;

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array_2);

      if Ics.Value (Item => Tc_Chars_Ptr) /=
        Ic.To_Ada (Ics.Value (Tc_Chars_Ptr), Trim_Nul => True) then
         Report.Failed ("Incorrect result from Function Value - 2");
      end if;

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array_3);
      Tc_String    := Ics.Value (Tc_Chars_Ptr);

      if Tc_String /= Tc_String_3 or Tc_String (Tc_String'Last) = Acl1.Nul then
         Report.Failed ("Incorrect result from Function Value - 3");
      end if;

      -- Check that the version of Function Value with a chars_ptr parameter
      -- and a size_t parameter that returns a String result returns the
      -- shorter of:
      --   1) a String of the first size_t number of characters, or
      --   2) a String of characters up to (but not including) the
      --      terminating nul.
      --

      -- Case 1 : Length parameter specifies a length shorter than total
      --          length.

      Tc_Chars_Ptr       := Ics.New_Char_Array (Tc_Char_Array_1);
      Tc_String          := "XXXXX";  -- Reinitialize all characters in string.
      Tc_String (1 .. 5) := Ics.Value (Item => Tc_Chars_Ptr, Length => 6);

      if Tc_String (1 .. 4) /= Tc_String_1 (1 .. 4) or
        Tc_String (Tc_String'Last) = Acl1.Nul then
         Report.Failed ("Incorrect result from Function Value - 4");
      end if;

      -- Case 2 : Length parameter specifies total length.

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array_2);

      if Ics.Value (Tc_Chars_Ptr, Length => 5) /=
        Ic.To_Ada (Ics.Value (Tc_Chars_Ptr), Trim_Nul => True) then
         Report.Failed ("Incorrect result from Function Value - 5");
      end if;

      -- Case 3 : Length parameter specifies a length longer than total
      --          length.

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array_3);
      Tc_String    := "XXXXX";  -- Reinitialize all characters in string.
      Tc_String    := Ics.Value (Tc_Chars_Ptr, 7);

      if Tc_String /= Tc_String_3 or Tc_String (Tc_String'Last) = Acl1.Nul then
         Report.Failed ("Incorrect result from Function Value - 6");
      end if;

      -- Check that the Function Strlen returns a size_t result that
      -- corresponds to the number of chars in the array pointed to
      -- by parameter Item, up to but not including the terminating nul.

      Tc_Chars_Ptr := Ics.New_Char_Array (Ic.To_C ("A longer string value"));
      Tc_Size_T    := Ics.Strlen (Tc_Chars_Ptr);

      if Tc_Size_T /= 21 then
         Report.Failed ("Incorrect result from Function Strlen - 1");
      end if;

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array_2);
      Tc_Size_T    := Ics.Strlen (Tc_Chars_Ptr);

      if Tc_Size_T /= 3 then  -- Nul not included in length.
         Report.Failed ("Incorrect result from Function Strlen - 2");
      end if;

      Tc_Chars_Ptr := Ics.New_Char_Array (Ic.To_C (""));
      Tc_Size_T    := Ics.Strlen (Tc_Chars_Ptr);

      if Tc_Size_T /= 0 then
         Report.Failed ("Incorrect result from Function Strlen - 3");
      end if;

      -- Check that both of the above versions of Function Value and function
      -- Strlen propagate Dereference_Error if the Item parameter is Null_Ptr.

      begin
         Tc_Chars_Ptr := Ics.Null_Ptr;
         Tc_String    := Ics.Value (Item => Tc_Chars_Ptr);
         Report.Failed
           ("Function Value (without Length parameter) did not " &
            "raise Dereference_Error when provided a null Item " &
            "parameter input value");
         if Tc_String (1) = '1' then   -- Defeat optimization.
            Report.Comment ("Should never be printed");
         end if;
      exception
         when Ics.Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Function Value " &
               "with Item parameter, when the Item parameter " &
               "is Null_Ptr");
      end;

      begin
         Tc_Chars_Ptr := Ics.Null_Ptr;
         Tc_String    := Ics.Value (Item => Tc_Chars_Ptr, Length => 4);
         Report.Failed
           ("Function Value (with Length parameter) did not " &
            "raise Dereference_Error when provided a null Item " &
            "parameter input value");
         if Tc_String (1) = '1' then   -- Defeat optimization.
            Report.Comment ("Should never be printed");
         end if;
      exception
         when Ics.Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Function Value " &
               "with both Item and Length parameters, when " &
               "the Item parameter is Null_Ptr");
      end;

      begin
         Tc_Chars_Ptr := Ics.Null_Ptr;
         Tc_Size_T    := Ics.Strlen (Item => Tc_Chars_Ptr);
         Report.Failed
           ("Function Strlen did not raise Dereference_Error" &
            "when provided a null Item parameter input value");
         if Tc_Size_T = 35 then   -- Defeat optimization.
            Report.Comment ("Should never be printed");
         end if;
      exception
         when Ics.Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Function Strlen " &
               "when the Item parameter is Null_Ptr");
      end;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxb3011;
