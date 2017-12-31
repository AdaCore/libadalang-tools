-- CXB3007.A
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
--      Check that the procedure To_C converts the Wide_Character elements
--      of a Wide_String parameter into wchar_t elements of the wchar_array
--      parameter Target, with wide_nul termination if parameter Append_Nul
--      is true.
--
--      Check that the out parameter Count of procedure To_C is set to the
--      appropriate value for both the wide_nul/no wide_nul terminated cases.
--
--      Check that Constraint_Error is propagated by procedure To_C if the
--      length of the wchar_array parameter Target is not sufficient to
--      hold the converted Wide_String value.
--
--      Check that the Procedure To_Ada converts wchar_t elements of the
--      wchar_array parameter Item to the corresponding Wide_Character
--      elements of Wide_String out parameter Target.
--
--      Check that Constraint_Error is propagated by Procedure To_Ada if the
--      length of Wide_String parameter Target is not long enough to hold the
--      converted wchar_array value.
--
--      Check that Terminator_Error is propagated by Procedure To_Ada if the
--      parameter Trim_Nul is set to True, but the actual Item parameter
--      contains no wide_nul wchar_t.
--
-- TEST DESCRIPTION:
--      This test uses a variety of Wide_String, and wchar_array objects to
--      test versions of the To_C and To_Ada procedures.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.wchar_t:
--      ' ', 'a'..'z', 'A'..'Z', and '-'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.C.  If an implementation provides
--      package Interfaces.C, this test must compile, execute, and
--      report "PASSED".
--
-- CHANGE HISTORY:
--      01 Sep 95   SAIC    Initial prerelease version.
--      09 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--      14 Sep 99   RLB     Removed incorrect and unnecessary
--                          Unchecked_Conversion.
--
--!

with Report;
with Interfaces.C;                                            -- N/A => ERROR
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Wide_Fixed;

procedure Cxb3007 is
begin

   Report.Test
     ("CXB3007",
      "Check that the procedures To_C and To_Ada " &
      "for wide strings produce correct results");
   Test_Block :
   declare

      use Interfaces, Interfaces.C;
      use Ada.Characters, Ada.Characters.Handling;
      use Ada.Exceptions;
      use Ada.Strings.Wide_Fixed;

      Tc_Short_Wide_String : Wide_String (1 .. 4) :=
        (others => Wide_Character'First);
      Tc_Wide_String : Wide_String (1 .. 8) :=
        (others => Wide_Character'First);
      Tc_Wchar_Array   : Wchar_Array (0 .. 7) := (others => Wchar_T'First);
      Tc_Size_T_Count  : Size_T               := Size_T'First;
      Tc_Natural_Count : Natural              := Natural'First;

      -- We can use the wide character forms of To_Ada and To_C here to check
      -- the results; they were tested in CXB3006. We give them different names
      -- to avoid confusion below.

      function Wide_Character_To_Wchar_T
        (Source : in Wide_Character) return Wchar_T renames
        To_C;
      function Wchar_T_To_Wide_Character
        (Source : in Wchar_T) return Wide_Character renames
        To_Ada;

   begin

      -- Check that the procedure To_C converts the Wide_Character elements
      -- of a Wide_String parameter into wchar_t elements of wchar_array out
      -- parameter Target.
      --
      -- Case of wide_nul termination.

      Tc_Wide_String (1 .. 6) := "abcdef";

      To_C
        (Item   => Tc_Wide_String (1 .. 6),  -- Source slice of length 6.
         Target => Tc_Wchar_Array,
         Count  => Tc_Size_T_Count, Append_Nul => True);

      -- Check that the out parameter Count is set to the appropriate value for
      -- the wide_nul terminated case.

      if Tc_Size_T_Count /= 7 then
         Report.Failed
           ("Incorrect setting of out parameter Count by " &
            "Procedure To_C when Append_Nul => True");
      end if;

      for I in 1 .. Tc_Size_T_Count - 1 loop
         if Wchar_T_To_Wide_Character (Tc_Wchar_Array (I - 1)) /=
           Tc_Wide_String (Integer (I)) then
            Report.Failed
              ("Incorrect result from Procedure To_C when " &
               "checking individual wchar_t values, case of " &
               "Append_Nul => True; " & "wchar_t position = " &
               Integer'Image (Integer (I)));
         end if;
      end loop;

      if not Is_Nul_Terminated (Tc_Wchar_Array) then
         Report.Failed
           ("No wide_nul wchar_t appended to the wchar_array " &
            "result from Procedure To_C when Append_Nul => True");
      end if;

      if Tc_Wchar_Array (0 .. 6) /= To_C ("abcdef", True) then
         Report.Failed
           ("Incorrect result from Procedure To_C when " &
            "directly comparing wchar_array results, case " &
            "of Append_Nul => True");
      end if;

      -- Check Procedure To_C with no wide_nul termination.

      Tc_Wchar_Array          := (others => Wide_Character_To_Wchar_T ('M'));
      Tc_Wide_String (1 .. 4) := "WXYZ";

      To_C
        (Item   => Tc_Wide_String (1 .. 4),  -- Source slice of length 4.
         Target => Tc_Wchar_Array,
         Count  => Tc_Size_T_Count, Append_Nul => False);

      -- Check that the out parameter Count is set to the appropriate value for
      -- the non-wide_nul terminated case.

      if Tc_Size_T_Count /= 4 then
         Report.Failed
           ("Incorrect setting of out parameter Count by " &
            "Procedure To_C when Append_Nul => False");
      end if;

      for I in 1 .. Tc_Size_T_Count loop
         if Wchar_T_To_Wide_Character (Tc_Wchar_Array (I - 1)) /=
           Tc_Wide_String (Integer (I)) then
            Report.Failed
              ("Incorrect result from Procedure To_C when " &
               "checking individual wchar_t values, case of " &
               "Append_Nul => False; " & "wchar_t position = " &
               Integer'Image (Integer (I)));
         end if;
      end loop;

      if Is_Nul_Terminated (Tc_Wchar_Array) then
         Report.Failed
           ("The wide_nul wchar_t was appended to the wchar_array " &
            "result of Procedure To_C when Append_Nul => False");
      end if;

      if Tc_Wchar_Array (0 .. 3) /= To_C ("WXYZ", False) then
         Report.Failed
           ("Incorrect result from Procedure To_C when " &
            "directly comparing wchar_array results, case " &
            "of Append_Nul => False");
      end if;

      -- Check that Constraint_Error is raised by procedure To_C if the length
      -- of the target wchar_array parameter is not sufficient to hold the
      -- converted Wide_String value (plus wide_nul if Append_Nul is True).

      Tc_Wchar_Array := (others => Wchar_T'First);
      begin
         To_C
           ("A string too long", Tc_Wchar_Array, Tc_Size_T_Count,
            Append_Nul => True);

         Report.Failed
           ("Constraint_Error not raised when the Target " &
            "parameter of Procedure To_C is not long enough " &
            "to hold the converted Wide_String");
         Report.Comment
           (To_Character (Wchar_T_To_Wide_Character (Tc_Wchar_Array (0))) &
            " printed to defeat optimization");
      exception
         when Constraint_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Procedure " &
               "To_C when the Target parameter is not long " &
               "enough to contain the wchar_array result");
      end;

      -- Check that the procedure To_Ada converts wchar_t elements of the
      -- wchar_array parameter Item to the corresponding Wide_Character
      -- elements of Wide_String out parameter Target, with result wide
      -- string length based on the Trim_Nul parameter.
      --
      -- Case of appended wide_nul wchar_t on the wchar_array In parameter.

      Tc_Wchar_Array := To_C ("ACVC-95", Append_Nul => True); -- 8 total chars.

      To_Ada
        (Item  => Tc_Wchar_Array, Target => Tc_Wide_String,
         Count => Tc_Natural_Count, Trim_Nul => False);

      if Tc_Natural_Count /= 8 then
         Report.Failed
           ("Incorrect value returned in out parameter Count " &
            "by Procedure To_Ada, case of Trim_Nul => False");
      end if;

      for I in 1 .. Tc_Natural_Count loop
         if Wide_Character_To_Wchar_T (Tc_Wide_String (I)) /=
           Tc_Wchar_Array (Size_T (I - 1)) then
            Report.Failed
              ("Incorrect result from Procedure To_Ada when " &
               "checking individual wchar_t values, case of " &
               "Trim_Nul => False, when a wide_nul is present " &
               "in the wchar_array input parameter; " & "position = " &
               Integer'Image (Integer (I)));
         end if;
      end loop;

      if Tc_Wide_String (Tc_Natural_Count) /= To_Wide_Character (Latin_1.Nul)
      then
         Report.Failed
           ("Last Wide_Character of Wide_String result of " &
            "Procedure To_Ada is not Nul, even though a " &
            "wide_nul was present in the wchar_array argument, " &
            "and the Trim_Nul parameter was set to False");
      end if;

      Tc_Wide_String          := (others => Wide_Character'First);
      Tc_Wchar_Array (0 .. 3) := To_C ("XYz", Append_Nul => True); -- 4 chars.

      To_Ada
        (Item  => Tc_Wchar_Array, Target => Tc_Wide_String,
         Count => Tc_Natural_Count, Trim_Nul => True);

      if Tc_Natural_Count /= 3 then
         Report.Failed
           ("Incorrect value returned in out parameter Count " &
            "by Procedure To_Ada, case of Trim_Nul => True");
      end if;

      for I in 1 .. Tc_Natural_Count loop
         if Wide_Character_To_Wchar_T (Tc_Wide_String (I)) /=
           Tc_Wchar_Array (Size_T (I - 1)) then
            Report.Failed
              ("Incorrect result from Procedure To_Ada when " &
               "checking individual wchar_t values, case of " &
               "Trim_Nul => True, when a wide_nul is present " &
               "in the wchar_array input parameter; " & "position = " &
               Integer'Image (Integer (I)));
         end if;
      end loop;

      if Tc_Wide_String (Tc_Natural_Count) = To_Wide_Character (Latin_1.Nul)
      then
         Report.Failed
           ("Last Wide_Character of Wide_String result of " &
            "Procedure To_Ada is  Nul, even though the " &
            "Trim_Nul parameter was set to True");
      end if;

      if Tc_Wide_String (Tc_Natural_Count + 1) /= Wide_Character'First then
         Report.Failed ("Incorrect replacement from To_Ada");
      end if;

      -- Case of no wide_nul wchar_t present in the wchar_array argument.

      Tc_Wide_String := (others => Wide_Character'First);
      Tc_Wchar_Array := To_C ("ABCDWXYZ", Append_Nul => False);

      To_Ada
        (Item  => Tc_Wchar_Array, Target => Tc_Wide_String,
         Count => Tc_Natural_Count, Trim_Nul => False);

      if Tc_Natural_Count /= 8 then
         Report.Failed
           ("Incorrect value returned in out parameter Count " &
            "by Procedure To_Ada, case of Trim_Nul => False, " &
            "with no wide_nul wchar_t present in the parameter " & "Item");
      end if;

      for I in 1 .. Tc_Natural_Count loop
         if Wide_Character_To_Wchar_T (Tc_Wide_String (I)) /=
           Tc_Wchar_Array (Size_T (I - 1)) then
            Report.Failed
              ("Incorrect result from Procedure To_Ada when " &
               "checking individual wchar_t values, case of " &
               "Trim_Nul => False, when a wide_nul is not " &
               "present in the wchar_array input parameter; " & "position = " &
               Integer'Image (Integer (I)));
         end if;
      end loop;

      if Tc_Wide_String (Tc_Natural_Count) = To_Wide_Character (Latin_1.Nul)
      then
         Report.Failed
           ("Last Wide_Character of Wide_String result of " &
            "Procedure To_Ada is Nul, even though the wide_nul " &
            "wchar_t was not present in the parameter Item, " &
            "with the parameter Trim_Nul => False");
      end if;

      -- Check that the Procedure To_Ada raises Terminator_Error if the
      -- parameter Trim_Nul is set to True, but the actual Item parameter
      -- does not contain the wide_nul wchar_t.

      begin
         Tc_Wide_String := (others => Wide_Character'First);
         Tc_Wchar_Array := To_C ("ABCDWXYZ", Append_Nul => False);

         To_Ada
           (Tc_Wchar_Array, Tc_Wide_String, Count => Tc_Natural_Count,
            Trim_Nul                              => True);

         Report.Failed
           ("Terminator_Error not raised when Item " &
            "parameter of To_Ada does not contain the " &
            "wide_nul wchar_t, but parameter Trim_Nul => True");
         Report.Comment
           (To_String (Tc_Wide_String) & " printed to defeat optimization");
      exception
         when Terminator_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Procedure " &
               "To_Ada when the Item parameter does not " &
               "contain the wide_nul wchar_t, but parameter " &
               "Trim_Nul => True");
      end;

      -- Check that Constraint_Error is propagated by procedure To_Ada if the
      -- length of Wide_String parameter Target is not long enough to hold the
      -- converted wchar_array value (plus wide_nul if Trim_Nul is False).

      begin
         Tc_Wchar_Array (0 .. 4) := To_C ("ABCD", Append_Nul => True);

         To_Ada
           (Tc_Wchar_Array (0 .. 4),
            Tc_Short_Wide_String, -- Length of 4.
            Count => Tc_Natural_Count, Trim_Nul => False);

         Report.Failed
           ("Constraint_Error not raised when Wide_String " &
            "parameter Target of Procedure To_Ada is not " &
            "long enough to hold the converted wchar_ts");
         Report.Comment
           (To_String (Tc_Short_Wide_String) &
            " printed to defeat optimization");
      exception
         when Constraint_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Procedure " &
               "To_Ada when Wide_String parameter Target is " &
               "not long enough to hold the converted wchar_ts");
      end;

   exception
      when The_Error : others =>
         Report.Failed
           ("The following exception was raised in the " & "Test_Block: " &
            Exception_Name (The_Error));
   end Test_Block;

   Report.Result;

end Cxb3007;
