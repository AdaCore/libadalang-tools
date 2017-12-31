-- CXB3012.A
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
--      Check that Procedure Update modifies the value pointed to by
--      the chars_ptr parameter Item, starting at the position
--      corresponding to parameter Offset, using the chars in
--      char_array parameter Chars.
--
--      Check that the version of Procedure Update with a String parameter
--      behaves in the manner described above, but with the character
--      values in the String overwriting the char values in Item.
--
--      Check that both of the above versions of Procedure Update will
--      propagate Update_Error if Check is True, and if the length of
--      the new chars in Chars, when overlaid starting from position
--      Offset, will overwrite the first nul in Item.
--
-- TEST DESCRIPTION:
--      This test checks two versions of Procedure Update.  In the first
--      version of the procedure, the parameter Chars indicates a char_array
--      argument.  These char_array parameters are provided through the use
--      of the To_C function (with String IN parameter), both with and
--      without a terminating nul.  In the case below where a terminating nul
--      char is appended, the effect of "updating" the value pointed to by the
--      Item parameter will include its shortening, due to the insertion of
--      this additional nul in the middle of the char_array.
--
--      In the second version of Procedure Update evaluated here, the string
--      parameter Str is used to modify the char_array pointed to by Item.
--
--      Finally, both versions of the procedure are evaluated to ensure that
--      they propagate Update_Error and Dereference_Error under the proper
--      conditions.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', 'a'..'z', 'A'..'Z', '0'..'9', '-' and '.'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.C.Strings.  If an implementation provides
--      package Interfaces.C.Strings, this test must compile, execute,
--      and report "PASSED".
--
--
-- CHANGE HISTORY:
--      05 Oct 95   SAIC    Initial prerelease version.
--      13 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--      14 Sep 99   RLB     Removed incorrect and unnecessary
--                          Unchecked_Conversion. Added check for raising
--                          of Dereference_Error for Update (From Technical
--                          Corrigendum 1).
--      07 Jan 05   RLB     Modified to reflect change to Update by AI-242
--                          (which is expected to be part of Amendment 1).
--      14 Mar 07   RLB     Modified to require Amendment behavior.
--!

with Report;
with Ada.Exceptions;
with Interfaces.C.Strings;                                    -- N/A => ERROR

procedure Cxb3012 is
begin

   Report.Test
     ("CXB3012",
      "Check that both versions of Procedure Update " &
      "produce correct results");

   Test_Block :
   declare

      package Ic renames Interfaces.C;
      package Ics renames Interfaces.C.Strings;
      use Ada.Exceptions;

      use type Ic.Char;
      use type Ic.Char_Array;
      use type Ic.Size_T;
      use type Ics.Chars_Ptr;

      Tc_String_1  : String (1 .. 1)  := "J";
      Tc_String_2  : String (1 .. 2)  := "Ab";
      Tc_String_3  : String (1 .. 3)  := "xyz";
      Tc_String_4  : String (1 .. 4)  := "ACVC";
      Tc_String_5  : String (1 .. 5)  := "1a2b3";
      Tc_String_6  : String (1 .. 6)  := "---...";
      Tc_String_7  : String (1 .. 7)  := "AABBBAA";
      Tc_String_8  : String (1 .. 8)  := "aBcDeFgH";
      Tc_String_9  : String (1 .. 9)  := "JustATest";
      Tc_String_10 : String (1 .. 10) := "0123456789";

      Tc_Result_String_1 : constant String := "JXXXXXXXXX";
      Tc_Result_String_2 : constant String := "XXXXXXXXAb";
      Tc_Result_String_3 : constant String := "XXXxyz";
      Tc_Result_String_4 : constant String := "XACVC";
      Tc_Result_String_5 : constant String := "1a2b3";
      Tc_Result_String_6 : constant String := "XXX---...";

      Tc_Amd_Result_String_4 : constant String := "XACVCXXXXX";
      Tc_Amd_Result_String_5 : constant String := "1a2b3XXXXX";
      Tc_Amd_Result_String_6 : constant String := "XXX---...X";
      Tc_Amd_Result_String_9 : constant String := "JustATestX";

      Tc_Char_Array        : Ic.Char_Array (0 .. 10) := Ic.To_C ("XXXXXXXXXX");
      Tc_Result_Char_Array : Ic.Char_Array (0 .. 10) := Ic.To_C ("XXXXXXXXXX");
      Tc_Chars_Ptr         : Ics.Chars_Ptr;
      Tc_Length            : Ic.Size_T;

   begin

      -- Check that Procedure Update modifies the value pointed to by the
      -- chars_ptr parameter Item, starting at the position corresponding to
      -- parameter Offset, using the chars in char_array parameter Chars. Note:
      -- If parameter Chars contains a nul char (such as a
      --       terminating nul), the result may be the overall shortening
      --       of parameter Item.

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);

      Ics.Update
        (Item   => Tc_Chars_Ptr,
         Offset => 0,
         Chars  => Ic.To_C (Tc_String_1, False),  -- No nul char.
         Check  => True);

      if Ics.Value (Tc_Chars_Ptr) /= Tc_Result_String_1 then
         Report.Failed ("Incorrect result from Procedure Update - 1");
      end if;
      Ics.Free (Tc_Chars_Ptr);

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
      Ics.Update
        (Tc_Chars_Ptr,
         Offset => Ics.Strlen (Tc_Chars_Ptr) - 2,
         Chars  => Ic.To_C (Tc_String_2, False),  -- No nul char.
         Check  => True);

      if Ics.Value (Tc_Chars_Ptr) /= Tc_Result_String_2 then
         Report.Failed ("Incorrect result from Procedure Update - 2");
      end if;
      Ics.Free (Tc_Chars_Ptr);

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
      Ics.Update
        (Tc_Chars_Ptr,
         3,
         Chars => Ic.To_C (Tc_String_3),   -- Nul appended, shortens
         Check => False);                 -- array.

      if Ics.Value (Tc_Chars_Ptr) /= Tc_Result_String_3 then
         Report.Failed ("Incorrect result from Procedure Update - 3");
      end if;
      Ics.Free (Tc_Chars_Ptr);

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
      Ics.Update
        (Tc_Chars_Ptr,
         0,
         Ic.To_C (Tc_String_10), -- Complete replacement of array.
         Check => False);

      if Ics.Value (Tc_Chars_Ptr) /= Tc_String_10 then
         Report.Failed ("Incorrect result from Procedure Update - 4");
      end if;

      -- Perform a character-by-character comparison of the result of Procedure
      -- Update. Note that char_array lower bound is 0, and that the nul char
      -- is not compared with any character in the string (since the string is
      -- not nul terminated).
      begin
         Tc_Length                      := Ics.Strlen (Tc_Chars_Ptr);
         Tc_Result_Char_Array (0 .. 10) := Ics.Value (Tc_Chars_Ptr);
         for I in 0 .. Tc_Length - 1 loop
            if Tc_Result_Char_Array (I) /=
              Ic.To_C (Tc_String_10 (Integer (I + 1))) then
               Report.Failed
                 ("Incorrect result from the character-by-" &
                  "character evaluation of the result of " &
                  "Procedure Update");
            end if;
         end loop;
      exception
         when others =>
            Report.Failed
              ("Exception raised during the character-by-" &
               "character evaluation of the result of " & "Procedure Update");
      end;
      Ics.Free (Tc_Chars_Ptr);

      -- Check that the version of Procedure Update with a String rather than
      -- a char_array parameter behaves in the manner described above, but
      -- with the character values in the String overwriting the char values
      -- in Item.
      --
      -- Note: In Ada 95, in each of the cases below, the String parameter
      --       Str is treated as if it were nul terminated, which means that
      --       the char_array pointed to by TC_chars_ptr will be "shortened"
      --       so that it ends after the last character of the Str
      --       parameter. This rule was dropped by Amendment 1, so the
      --       number of characters remains the same.

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
      Ics.Update (Tc_Chars_Ptr, 1, Tc_String_4, False);

      if Ics.Value (Tc_Chars_Ptr) = Tc_Result_String_4 then
         Report.Failed ("Ada 95 result from Procedure Update - 5");
      elsif Ics.Value (Tc_Chars_Ptr) = Tc_Amd_Result_String_4 then
         null; -- OK
      else
         Report.Failed ("Incorrect result from Procedure Update - 5");
      end if;
      Ics.Free (Tc_Chars_Ptr);

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
      Ics.Update (Item => Tc_Chars_Ptr, Offset => 0, Str => Tc_String_5);

      if Ics.Value (Tc_Chars_Ptr) = Tc_Result_String_5 then
         Report.Failed ("Ada 95 result from Procedure Update - 6");
      elsif Ics.Value (Tc_Chars_Ptr) = Tc_Amd_Result_String_5 then
         null; -- OK
      else
         Report.Failed ("Incorrect result from Procedure Update - 6");
      end if;
      Ics.Free (Tc_Chars_Ptr);

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
      Ics.Update (Tc_Chars_Ptr, 3, Str => Tc_String_6, Check => True);

      if Ics.Value (Tc_Chars_Ptr) = Tc_Result_String_6 then
         Report.Failed ("Ada 95 result from Procedure Update - 7");
      elsif Ics.Value (Tc_Chars_Ptr) = Tc_Amd_Result_String_6 then
         null; -- OK
      else
         Report.Failed ("Incorrect result from Procedure Update - 7");
      end if;
      Ics.Free (Tc_Chars_Ptr);

      Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
      Ics.Update (Tc_Chars_Ptr, 0, Tc_String_9, True);

      if Ics.Value (Tc_Chars_Ptr) = Tc_String_9 then
         Report.Failed ("Ada 95 result from Procedure Update - 8");
      elsif Ics.Value (Tc_Chars_Ptr) = Tc_Amd_Result_String_9 then
         null; -- OK
      else
         Report.Failed ("Incorrect result from Procedure Update - 8");
      end if;
      Ics.Free (Tc_Chars_Ptr);

      -- Check what happens if the string and array are the same size (this is
      -- the case that caused the change made by the Amendment).
      begin
         Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
         Ics.Update
           (Item  => Tc_Chars_Ptr, Offset => 0, Str => Tc_String_10,
            Check => True);
         if Ics.Value (Tc_Chars_Ptr) = Tc_String_10 then
            null; -- OK
         else
            Report.Failed ("Incorrect result from Procedure Update - 9");
         end if;
      exception
         when Ics.Update_Error =>
            Report.Failed
              ("Ada 95 exception expected from Procedure Update - 9");
         when others =>
            Report.Failed
              ("Incorrect exception raised by Procedure Update " &
               "with Str parameter - 9");
      end;
      Ics.Free (Tc_Chars_Ptr);

      -- Check that both of the above versions of Procedure Update will
      -- propagate Update_Error if Check is True, and if the length of the
      -- new chars in Chars, when overlaid starting from position Offset,
      -- will overwrite the first nul in Item.

      begin
         Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
         Ics.Update
           (Item  => Tc_Chars_Ptr, Offset => 5, Chars => Ic.To_C (Tc_String_7),
            Check => True);
         Report.Failed
           ("Update_Error not raised by Procedure Update with " &
            "Chars parameter");
         Report.Comment
           (Ics.Value (Tc_Chars_Ptr) & "used here to defeat " &
            "optimization - should never be printed");
      exception
         when Ics.Update_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Procedure Update " &
               "with Chars parameter");
      end;

      Ics.Free (Tc_Chars_Ptr);

      begin
         Tc_Chars_Ptr := Ics.New_Char_Array (Tc_Char_Array);
         Ics.Update
           (Item => Tc_Chars_Ptr, Offset => Ics.Strlen (Tc_Chars_Ptr),
            Str  => Tc_String_8); -- Default Check parameter value.
         Report.Failed
           ("Update_Error not raised by Procedure Update with " &
            "Str parameter");
         Report.Comment
           (Ics.Value (Tc_Chars_Ptr) & "used here to defeat " &
            "optimization - should never be printed");
      exception
         when Ics.Update_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Procedure Update " &
               "with Str parameter");
      end;

      Ics.Free (Tc_Chars_Ptr);

      -- Check that both of the above versions of Procedure Update will
      -- propagate Dereference_Error if Item is Null_Ptr. Note: Free sets
      -- TC_chars_ptr to Null_Ptr.

      begin
         Ics.Update
           (Item  => Tc_Chars_Ptr, Offset => 5, Chars => Ic.To_C (Tc_String_7),
            Check => True);
         Report.Failed
           ("Dereference_Error not raised by Procedure Update with " &
            "Chars parameter");
      exception
         when Ics.Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Procedure Update " &
               "with Chars parameter");
      end;

      begin
         Ics.Update
           (Item => Tc_Chars_Ptr, Offset => Ics.Strlen (Tc_Chars_Ptr),
            Str  => Tc_String_8); -- Default Check parameter value.
         Report.Failed
           ("Dereference_Error not raised by Procedure Update with " &
            "Str parameter");
      exception
         when Ics.Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Procedure Update " &
               "with Str parameter");
      end;

   exception
      when The_Error : others =>
         Report.Failed
           ("The following exception was raised in the " & "Test_Block: " &
            Exception_Name (The_Error));
   end Test_Block;

   Report.Result;

end Cxb3012;
