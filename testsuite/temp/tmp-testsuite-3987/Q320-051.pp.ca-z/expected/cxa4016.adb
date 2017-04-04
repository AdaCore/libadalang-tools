-- CXA4016.A
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
--      Check that the subprograms defined in package Ada.Strings.Wide_Fixed
--      are available, and that they produce correct results.  Specifically,
--      check the subprograms Delete, Head, Insert, Overwrite, Replace_Slice,
--      Tail, Trim, and "*".
--
-- TEST DESCRIPTION:
--      This test, when combined with tests CXA4013-15 will provide
--      coverage of the functionality found in package Ada.Strings.Wide_Fixed.
--      This test contains many small, specific test cases, situations that
--      although common in user environments, are often difficult to generate
--      in large numbers in a application-based test.  They represent
--      individual usage paradigms in-the-small.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      10 Apr 94   SAIC    Modified comments in a subtest failure message.
--      06 Nov 95   SAIC    Corrected subtest results for ACVC 2.0.1
--      14 Mar 01   RLB     Added checks that the lower bound is 1, similar
--                          to CXA4005. These changes were made to test
--                          Defect Report 8652/0049, as reflected in
--                          Technical Corrigendum 1.
--
--!

with Report;
with Ada.Strings;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps;

procedure Cxa4016 is

   type Tc_Name_Holder is access String;
   Name : Tc_Name_Holder;

   function Tc_Check (S : Wide_String) return Wide_String is
   begin
      if S'First /= 1 then
         Report.Failed
           ("Lower bound of result of function " &
            Name.all &
            " is" &
            Integer'Image (S'First));
      end if;
      return S;
   end Tc_Check;

   procedure Tc_Set_Name (N : String) is
   begin
      Name := new String'(N);
   end Tc_Set_Name;

begin

   Report.Test
     ("CXA4016",
      "Check that the subprograms defined in " &
      "package Ada.Strings.Wide_Fixed are available, " &
      "and that they produce correct results");

   Test_Block : declare

      package Asw renames Ada.Strings.Wide_Fixed;
      package Wide_Maps renames Ada.Strings.Wide_Maps;

      Result_String,
      Delete_String,
      Insert_String,
      Trim_String,
      Overwrite_String : Wide_String (1 .. 10) :=
        (others => Ada.Strings.Wide_Space);
      Replace_String : Wide_String (10 .. 30) :=
        (others => Ada.Strings.Wide_Space);

      Source_String1 : Wide_String (1 .. 5)  := "abcde";     -- odd len wd str
      Source_String2 : Wide_String (1 .. 6)  := "abcdef";    -- even len wd str
      Source_String3 : Wide_String (1 .. 12) := "abcdefghijkl";
      Source_String4 : Wide_String (1 .. 12) :=
        "abcdefghij  "; -- last two ch pad
      Source_String5 : Wide_String (1 .. 12) :=
        "  cdefghijkl"; -- first two ch pad
      Source_String6 : Wide_String (1 .. 12) := "abcdefabcdef";

      Location               : Natural := 0;
      Slice_Start            : Positive;
      Slice_End, Slice_Count : Natural := 0;

      Cd_Set     : Wide_Maps.Wide_Character_Set := Wide_Maps.To_Set ("cd");
      X_Set      : Wide_Maps.Wide_Character_Set := Wide_Maps.To_Set ('x');
      Abcd_Set   : Wide_Maps.Wide_Character_Set := Wide_Maps.To_Set ("abcd");
      A_To_F_Set : Wide_Maps.Wide_Character_Set := Wide_Maps.To_Set ("abcdef");

      Cd_To_Xy_Map : Wide_Maps.Wide_Character_Mapping :=
        Wide_Maps.To_Mapping (From => "cd", To => "xy");

   begin

      -- Procedure Replace_Slice
      -- The functionality of this procedure is similar to procedure Move,
      -- and is tested here in the same manner, evaluated with various
      -- combinations of parameters.

      -- Index_Error propagation when Low > Source'Last + 1

      begin
         Asw.Replace_Slice
           (Result_String,
            Result_String'Last + 2, -- should raise exception
            Result_String'Last,
            "xxxxxxx");
         Report.Failed ("Index_Error not raised by Replace_Slice - 1");
      exception
         when Ada.Strings.Index_Error =>
            null; -- OK, expected exception.
         when others =>
            Report.Failed ("Incorrect exception from Replace_Slice - 1");
      end;

      -- Index_Error propagation when High < Source'First - 1

      begin
         Asw.Replace_Slice
           (Replace_String (20 .. 30),
            Replace_String'First,
            Replace_String'First - 2, -- should raise exception
            "xxxxxxx");
         Report.Failed ("Index_Error not raised by Replace_Slice - 2");
      exception
         when Ada.Strings.Index_Error =>
            null; -- OK, expected exception.
         when others =>
            Report.Failed ("Incorrect exception from Replace_Slice - 2");
      end;

      -- Justify = Left (default case)

      Result_String := "XXXXXXXXXX";

      Asw.Replace_Slice
        (Source => Result_String,
         Low    => 1,
         High   => 10,
         By     => Source_String1);   -- "abcde"

      if Result_String /= "abcde     " then
         Report.Failed
           ("Incorrect result from Replace_Slice - Justify = Left");
      end if;

      -- Justify = Right

      Asw.Replace_Slice
        (Source  => Result_String,
         Low     => 1,
         High    => Result_String'Last,
         By      => Source_String2,      -- "abcdef"
         Drop    => Ada.Strings.Error,
         Justify => Ada.Strings.Right);

      if Result_String /= "    abcdef" then
         Report.Failed ("Incorrect result from Replace_Slice - Justify=Right");
      end if;

      -- Justify = Center (two cases, odd and even pad lengths)

      Asw.Replace_Slice
        (Result_String,
         1,
         Result_String'Last,
         Source_String1,      -- "abcde"
         Ada.Strings.Error,
         Ada.Strings.Center,
         'x');                -- non-default padding.

      if Result_String /= "xxabcdexxx" then  -- Unequal padding added right
         Report.Failed
           ("Incorrect result, Replace_Slice - Justify=Center - 1");
      end if;

      Asw.Replace_Slice
        (Result_String,
         1,
         Result_String'Last,
         Source_String2,        -- "abcdef"
         Ada.Strings.Error,
         Ada.Strings.Center);

      if Result_String /= "  abcdef  " then    -- Equal padding added on L/R.
         Report.Failed
           ("Incorrect result from Replace_Slice with " &
            "Justify = Center - 2");
      end if;

      -- When the source string is longer than the target string, several
      -- cases can be examined, with the results depending on the value of
      -- the Drop parameter.

      -- Drop = Left

      Asw.Replace_Slice
        (Result_String,
         1,
         Result_String'Last,
         Source_String3,            -- "abcdefghijkl"
         Drop => Ada.Strings.Left);

      if Result_String /= "cdefghijkl" then
         Report.Failed ("Incorrect result from Replace_Slice - Drop=Left");
      end if;

      -- Drop = Right

      Asw.Replace_Slice
        (Result_String,
         1,
         Result_String'Last,
         Source_String3,        -- "abcdefghijkl"
         Ada.Strings.Right);

      if Result_String /= "abcdefghij" then
         Report.Failed ("Incorrect result, Replace_Slice with Drop=Right");
      end if;

      -- Drop = Error

      -- The effect in this case depends on the value of the justify
      -- parameter, and on whether any characters in Source other than
      -- Pad would fail to be copied.

      -- Drop = Error, Justify = Left, right overflow characters are pad.

      Asw.Replace_Slice
        (Result_String,
         1,
         Result_String'Last,
         Source_String4,               -- "abcdefghij  "
         Drop    => Ada.Strings.Error,
         Justify => Ada.Strings.Left);

      if not (Result_String = "abcdefghij") then  -- leftmost 10 characters
         Report.Failed ("Incorrect result, Replace_Slice - Drop = Error - 1");
      end if;

      -- Drop = Error, Justify = Right, left overflow characters are pad.

      Asw.Replace_Slice
        (Source  => Result_String,
         Low     => 1,
         High    => Result_String'Last,
         By      => Source_String5,     -- "  cdefghijkl"
         Drop    => Ada.Strings.Error,
         Justify => Ada.Strings.Right);

      if Result_String /= "cdefghijkl" then  -- rightmost 10 characters
         Report.Failed ("Incorrect result, Replace_Slice - Drop = Error - 2");
      end if;

      -- In other cases of Drop=Error, Length_Error is propagated, such as:

      begin

         Asw.Replace_Slice
           (Source => Result_String,
            Low    => 1,
            High   => Result_String'Last,
            By     => Source_String3,    -- "abcdefghijkl"
            Drop   => Ada.Strings.Error);

         Report.Failed ("Length_Error not raised by Replace_Slice - 1");

      exception
         when Ada.Strings.Length_Error =>
            null;   -- OK
         when others =>
            Report.Failed ("Incorrect exception from Replace_Slice - 3");
      end;

      -- Function Replace_Slice

      Tc_Set_Name ("Replace_Slice");

      if Tc_Check (Asw.Replace_Slice ("abcde", 3, 3, "x")) /=
        "abxde" or -- High = Low
        Tc_Check (Asw.Replace_Slice ("abc", 2, 3, "xyz")) /= "axyz" or
        Tc_Check (Asw.Replace_Slice ("abcd", 4, 1, "xy")) /=
          "abcxyd" or -- High < Low
        Tc_Check (Asw.Replace_Slice ("abc", 2, 3, "x")) /= "ax" or
        Tc_Check (Asw.Replace_Slice ("a", 1, 1, "z")) /= "z"
      then
         Report.Failed ("Incorrect result from Function Replace_Slice - 1");
      end if;

      if Tc_Check (Asw.Replace_Slice ("abcde", 5, 5, "z")) /=
        "abcdz" or  -- By length 1
        Tc_Check (Asw.Replace_Slice ("abc", 1, 3, "xyz")) /=
          "xyz" or  -- High > Low
        Tc_Check (Asw.Replace_Slice ("abc", 3, 2, "xy")) /=
          "abxyc" or  -- insert
        Tc_Check (Asw.Replace_Slice ("a", 1, 1, "xyz")) /= "xyz"
      then
         Report.Failed ("Incorrect result from Function Replace_Slice - 2");
      end if;

      -- Function Insert.

      Tc_Set_Name ("Insert");

      declare
         New_String : constant Wide_String :=
           Tc_Check
             (Asw.Insert
                (Source   => Source_String1 (2 .. 5),    -- "bcde"
                 Before   => 2,
                 New_Item => Source_String2));         -- "abcdef"
      begin
         if New_String /= "abcdefbcde" then
            Report.Failed ("Incorrect result from Function Insert - 1");
         end if;
      end;

      if Tc_Check (Asw.Insert ("a", 1, "z")) /= "za" or
        Tc_Check (Asw.Insert ("abc", 3, "")) /= "abc" or
        Tc_Check (Asw.Insert ("abc", 4, "z")) /= "abcz"
      then
         Report.Failed ("Incorrect result from Function Insert - 2");
      end if;

      begin
         if Tc_Check
             (Asw.Insert
                (Source   => Source_String1 (2 .. 5), -- "bcde"
                 Before   => Report.Ident_Int (7),
                 New_Item => Source_String2))         -- "abcdef"
                 /=
           "babcdefcde"
         then
            Report.Failed ("Index_Error not raised by Insert - 3A");
         else
            Report.Failed ("Index_Error not raised by Insert - 3B");
         end if;
      exception
         when Ada.Strings.Index_Error =>
            null; -- OK, expected exception.
         when others =>
            Report.Failed ("Incorrect exception from Insert - 3");
      end;

      -- Procedure Insert

      -- Drop = Right

      Asw.Insert
        (Source   => Insert_String,
         Before   => 6,
         New_Item => Source_String2,       -- "abcdef"
         Drop     => Ada.Strings.Right);

      if Insert_String /= "     abcde" then  -- last char of New_Item dropped.
         Report.Failed ("Incorrect result from Insert with Drop = Right");
      end if;

      -- Drop = Left

      Asw.Insert
        (Source   => Insert_String,     -- 10 char string
         Before   => 2,                 -- 9 chars, 2..10 available
         New_Item => Source_String3,    -- 12 characters long.
         Drop     => Ada.Strings.Left); -- truncate from Left.

      if Insert_String /= "l    abcde" then     -- 10 chars, leading blank.
         Report.Failed ("Incorrect result from Insert with Drop=Left");
      end if;

      -- Drop = Error

      begin
         Asw.Insert
           (Source   => Result_String,       -- 10 chars
            Before   => Result_String'Last,
            New_Item => "abcdefghijk",
            Drop     => Ada.Strings.Error);
         Report.Failed ("Exception not raised by Procedure Insert");
      exception
         when Ada.Strings.Length_Error =>
            null; -- OK, expected exception
         when others =>
            Report.Failed ("Incorrect exception raised by Procedure Insert");
      end;

      -- Function Overwrite

      Tc_Set_Name ("Overwrite");

      Overwrite_String :=
        Tc_Check (Asw.Overwrite (Result_String,  -- 10 chars
        1,              -- starting at pos=1
        Source_String3 (1 .. 10)));

      if Overwrite_String /= Source_String3 (1 .. 10) then
         Report.Failed ("Incorrect result from Function Overwrite - 1");
      end if;

      if Tc_Check (Asw.Overwrite ("abcdef", 4, "xyz")) /= "abcxyz" or
        Tc_Check (Asw.Overwrite ("a", 1, "xyz")) /= "xyz" or  -- chars appended
        Tc_Check (Asw.Overwrite ("abc", 3, "   ")) /=
          "ab   " or  -- blanks appended
        Tc_Check (Asw.Overwrite ("abcde", 1, "z")) /= "zbcde"
      then
         Report.Failed ("Incorrect result from Function Overwrite - 2");
      end if;

      -- Procedure Overwrite, with truncation.

      Asw.Overwrite
        (Source   => Overwrite_String,    -- 10 characters.
         Position => 1,
         New_Item => Source_String3,      -- 12 characters.
         Drop     => Ada.Strings.Left);

      if Overwrite_String /= "cdefghijkl" then
         Report.Failed ("Incorrect result from Overwrite with Drop=Left");
      end if;

      -- The default drop value is Right, used here.

      Asw.Overwrite
        (Source   => Overwrite_String,    -- 10 characters.
         Position => 1,
         New_Item => Source_String3);     -- 12 characters.

      if Overwrite_String /= "abcdefghij" then
         Report.Failed ("Incorrect result from Overwrite with Drop=Right");
      end if;

      -- Drop = Error

      begin
         Asw.Overwrite
           (Source   => Overwrite_String,    -- 10 characters.
            Position => 1,
            New_Item => Source_String3,      -- 12 characters.
            Drop     => Ada.Strings.Error);
         Report.Failed ("Exception not raised by Procedure Overwrite");
      exception
         when Ada.Strings.Length_Error =>
            null; -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Procedure Overwrite");
      end;

      Overwrite_String := "ababababab";
      Asw.Overwrite (Overwrite_String, Overwrite_String'Last, "z");
      Asw.Overwrite (Overwrite_String, Overwrite_String'First, "z");
      Asw.Overwrite (Overwrite_String, 5, "zz");

      if Overwrite_String /= "zbabzzabaz" then
         Report.Failed ("Incorrect result from Procedure Overwrite");
      end if;

      -- Function Delete

      Tc_Set_Name ("Delete");

      declare
         New_String1 : constant Wide_String :=  -- Returns a 4 char wide str.
           Tc_Check
             (Asw.Delete (Source => Source_String3, From => 3, Through => 10));
         New_String2 : constant Wide_String :=    -- This returns Source.
           Tc_Check (Asw.Delete (Source_String3, 10, 3));
      begin
         if New_String1 /= "abkl" or New_String2 /= Source_String3 then
            Report.Failed ("Incorrect result from Function Delete - 1");
         end if;
      end;

      if Tc_Check (Asw.Delete ("a", 1, 1)) /= "" or     -- Source length = 1
        Tc_Check (Asw.Delete ("abc", 1, 2)) /=
          "c" or     -- From = Source'First
        Tc_Check (Asw.Delete ("abc", 3, 3)) /=
          "ab" or     -- From = Source'Last
        Tc_Check (Asw.Delete ("abc", 3, 1)) /= "abc"        -- From > Through
      then
         Report.Failed ("Incorrect result from Function Delete - 2");
      end if;

      -- Procedure Delete

      -- Justify = Left

      Delete_String := Source_String3 (1 .. 10);  -- Initialize to "abcdefghij"

      Asw.Delete
        (Source  => Delete_String,
         From    => 6,
         Through => Delete_String'Last,
         Justify => Ada.Strings.Left,
         Pad     => 'x');              -- pad with char 'x'

      if Delete_String /= "abcdexxxxx" then
         Report.Failed ("Incorrect result from Delete - Justify = Left");
      end if;

      -- Justify = Right

      Asw.Delete
        (Source  => Delete_String,      -- Remove x"s from end and
         From    => 6,                  -- shift right.
         Through => Delete_String'Last,
         Justify => Ada.Strings.Right,
         Pad     => 'x');               -- pad with char 'x' on left.

      if Delete_String /= "xxxxxabcde" then
         Report.Failed ("Incorrect result from Delete - Justify = Right");
      end if;

      -- Justify = Center

      Asw.Delete
        (Source  => Delete_String,
         From    => 1,
         Through => 5,
         Justify => Ada.Strings.Center,
         Pad     => 'z');

      if Delete_String /= "zzabcdezzz" then  -- extra pad char on right side.
         Report.Failed ("Incorrect result from Delete - Justify = Center");
      end if;

      -- Function Trim
      -- Use non-identity character sets to perform the trim operation.

      Tc_Set_Name ("Trim");

      Trim_String := "cdabcdefcd";

      -- Remove the "cd" from each end of the string.  This will not effect
      -- the "cd" slice at 5..6.

      declare
         New_String : constant Wide_String :=
           Tc_Check
             (Asw.Trim
                (Source => Trim_String,
                 Left   => Cd_Set,
                 Right  => Cd_Set));
      begin
         if New_String /= Source_String2 then    -- string "abcdef"
            Report.Failed
              ("Incorrect result from Trim with wide character sets");
         end if;
      end;

      if Tc_Check
          (Asw.Trim ("abcdef", Wide_Maps.Null_Set, Wide_Maps.Null_Set)) /=
        "abcdef"
      then
         Report.Failed ("Incorrect result from Trim with Null sets");
      end if;

      if Tc_Check (Asw.Trim ("cdxx", Cd_Set, X_Set)) /= "" then
         Report.Failed ("Incorrect result from Trim, wide string removal");
      end if;

      -- Procedure Trim

      -- Justify = Right

      Asw.Trim
        (Source  => Trim_String,
         Left    => Cd_Set,
         Right   => Cd_Set,
         Justify => Ada.Strings.Right,
         Pad     => 'x');

      if Trim_String /= "xxxxabcdef" then
         Report.Failed ("Incorrect result from Trim with Justify = Right");
      end if;

      -- Justify = Left

      Asw.Trim
        (Source  => Trim_String,
         Left    => X_Set,
         Right   => Wide_Maps.Null_Set,
         Justify => Ada.Strings.Left,
         Pad     => ' ');

      if Trim_String /= "abcdef    " then  -- Padded with 4 blanks on right.
         Report.Failed ("Incorrect result from Trim with Justify = Left");
      end if;

      -- Justify = Center

      Asw.Trim
        (Source  => Trim_String,
         Left    => Abcd_Set,
         Right   => Cd_Set,
         Justify => Ada.Strings.Center,
         Pad     => 'x');

      if Trim_String /= "xxef    xx" then  -- Padded with 4 pad chars on L/R
         Report.Failed ("Incorrect result from Trim with Justify = Center");
      end if;

      -- Function Head, testing use of padding.

      Tc_Set_Name ("Head");

      -- Use the wide characters of Source_String1 ("abcde") and pad the
      -- last five wide characters of Result_String with 'x' wide characters.

      Result_String := Tc_Check (Asw.Head (Source_String1, 10, 'x'));

      if Result_String /= "abcdexxxxx" then
         Report.Failed ("Incorrect result from Function Head with padding");
      end if;

      if Tc_Check (Asw.Head ("  ab  ", 2)) /= "  " or
        Tc_Check (Asw.Head ("a", 6, 'A')) /= "aAAAAA" or
        Tc_Check (Asw.Head (Asw.Head ("abc  ", 7, 'x'), 10, 'X')) /=
          "abc  xxXXX"
      then
         Report.Failed ("Incorrect result from Function Head");
      end if;

      -- Function Tail, testing use of padding.

      Tc_Set_Name ("Tail");

      -- Use the wide characters of Source_String1 ("abcde") and pad the
      -- first five wide characters of Result_String with 'x' wide characters.

      Result_String := Tc_Check (Asw.Tail (Source_String1, 10, 'x'));

      if Result_String /= "xxxxxabcde" then
         Report.Failed ("Incorrect result from Function Tail with padding");
      end if;

      if Tc_Check (Asw.Tail ("abcde  ", 5)) /= "cde  " or  -- blanks, back
        Tc_Check (Asw.Tail ("  abc ", 8, ' ')) /=
          "    abc " or  -- blanks, front/back
        Tc_Check (Asw.Tail ("", 5, 'Z')) /= "ZZZZZ" or  -- pad characters only
        Tc_Check (Asw.Tail ("abc", 0)) /= "" or  -- null result
        Tc_Check (Asw.Tail (Asw.Tail (" abc ", 6, 'x'), 10, 'X')) /=
          "XXXXx abc "
      then
         Report.Failed ("Incorrect result from Function Tail");
      end if;

      -- Function "*"  - with (Natural, Wide_String) parameters

      Tc_Set_Name ("""*""");

      if Tc_Check (Asw."*" (3, Source_String1)) /= "abcdeabcdeabcde" or
        Tc_Check (Asw."*" (2, Source_String2)) /= Source_String6 or
        Tc_Check (Asw."*" (4, Source_String1 (1 .. 2))) /= "abababab" or
        Tc_Check (Asw."*" (0, Source_String1)) /= ""
      then
         Report.Failed
           ("Incorrect result from Function ""*"" with wide strings");
      end if;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxa4016;
