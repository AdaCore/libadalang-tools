-- CZ00004.A
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
--      Check that Impdef values have been supplied for the special needs
--      annexes.  Check that the routines in TCTouch work correctly.
--      Check that the Ident_Wide functions in Report work correctly.
--      Display all values in Impdef.
--
-- TEST DESCRIPTION:
--      This test calls each of the interface subprograms in TCTouch.
--      This test checks that Report.Ident_Wide_Str and Report.Ident_Wide_Char
--      return the identity value.
--
--      This test checks that support routines used by the Ada Compiler
--      Validation Capability Suite (ACVC Suite) produce the expected
--      behaviors.  The validation suite assumes specific behavior from
--      the support code; this behavior is checked by the "CZ" tests.
--
--      If all the tested support code behaves correctly, the final test
--      result will report "FAILED".  Note that testing will provoke some
--      of the support routines to produce messages that indicate failure.
--      This is expected behavior in this test.  If the "FAILED" result is
--      produced, the test output must be examined for correctness.  The
--      following must be present:
--         1) expected results messages from support routines
--         2) report of which special needs annexes (if any) will be
--            supported for this validation
--         3) values of ImpDef objects
--      Results (2) and (3) allow AVFs to determine that
--      ImpDef has been properly modified for a validation of this
--      implementation.
--
--      Sample expected output for CZ00004 is included in the ACVC User's
--      Guide.
--
--      This test displays all displayable values in Impdef, and checks for
--      the existence of non-displayable values; the actual output will vary
--      according to the implementation.  This is intended as an "up front"
--      double check that all expected values are available in the Impdef
--      package.
--
-- PASS/FAIL CRITERIA:
--      This test passes if the output matches that supplied in the
--      applicable version of the ACVC users' guide.  A result of "FAILED"
--      is expected, but is not sufficient to pass this test.
--
-- CHANGE HISTORY:
--      26 FEB 96   SAIC   Initial version
--      12 NOV 96   SAIC   Typographical corrections to commentary
--      13 DEC 96   SAIC   Updated for 2.1
--
--      13 DEC 96   Keith  Removed references to Impdef values that
--                         moved to annex specific children.
--!

------------------------------------------------------------------- CZ00004

with Report;
with Impdef;
with Tctouch;
with Ada.Text_Io;
procedure Cz00004 is
   Wstring : Wide_String (1 .. 5);
   Wch     : Wide_Character;

   package Dio is new Ada.Text_Io.Fixed_Io (Duration);

   procedure New_Line is
   begin
      Ada.Text_Io.New_Line;
   end New_Line;

   procedure Comment (Message : String) is
   begin
      Ada.Text_Io.Put ("   - CZ00004 " & Message);
   end Comment;

begin  -- Main test procedure.

   Report.Test
     ("CZ00004",
      "Check that Impdef values have been supplied " &
      "for the special needs annexes.  Check that " &
      "the routines in TCTouch work correctly");

   Wch     := Wide_Character'Val (1_040);
   Wstring := Wch & Wch & Wch & Wch & Wide_Character'Val (1_099);

   if Report.Ident_Wide_Char (Wch) /= Wide_Character'Val (1_040) then
      Report.Failed
        ("Wide Character Identity Mismatch");            -- No output
   end if;

   if Report.Ident_Wide_Str (Wstring) /= Wstring then
      Report.Failed
        ("Wide String Identity Mismatch");               -- No output
   end if;

   Report.Comment (Tctouch.Foundation_Id);                        -- Output

   -- check the assertion interfaces
   Tctouch.Assert (False, "Assertion Failed is expected");        -- Output
   Tctouch.Assert_Not (True, "Assertion Failed is expected");     -- Output

   -- check that case is meaningful
   Tctouch.Touch ('Z');
   Tctouch.Validate ("z", "z should not equal Z");                -- Output

   Report.Comment ("Three failure messages should have occurred so far");

   Tctouch.Touch ('a');
   Tctouch.Touch ('z');
   Tctouch.Touch ('A');
   Tctouch.Touch ('Z');

   -- check that Order_Meaningful => False is exactly that
   Tctouch.Validate
     ("ZzAa", "Order should not count, " &          -- No output
     "this message is a true fail",
      False);

   -- check that touch overflow happens at the right time.
   for I in 1 .. Tctouch.Max_Touch_Count loop
      Tctouch.Touch ('x');
   end loop;

   Tctouch.Touch ('z');                                           -- Output

   Report.Comment ("A Trace Overflow message should have just occurred");

   -- check (indirectly) the settings for Impdef.Validating_Annex_X
   Report.Comment ("<><><><><> ANNEX VALIDATION STATUS <><><><><>");

   Tctouch.Implementation_Check ("Annex C validation:", Tctouch.Annex_C);
   Tctouch.Implementation_Check ("Annex D validation:", Tctouch.Annex_D);
   Tctouch.Implementation_Check ("Annex E validation:", Tctouch.Annex_E);
   Tctouch.Implementation_Check ("Annex F validation:", Tctouch.Annex_F);
   Tctouch.Implementation_Check ("Annex G validation:", Tctouch.Annex_G);
   Tctouch.Implementation_Check ("Annex H validation:", Tctouch.Annex_H);

   -- check Impdef
   Report.Comment ("<><><><><> IMPDEF <><><><><>");

   Report.Comment
     ("Validating_Annex_C : " & Boolean'Image (Impdef.Validating_Annex_C));
   Report.Comment
     ("Validating_Annex_D : " & Boolean'Image (Impdef.Validating_Annex_D));
   Report.Comment
     ("Validating_Annex_E : " & Boolean'Image (Impdef.Validating_Annex_E));
   Report.Comment
     ("Validating_Annex_F : " & Boolean'Image (Impdef.Validating_Annex_F));
   Report.Comment
     ("Validating_Annex_G : " & Boolean'Image (Impdef.Validating_Annex_G));
   Report.Comment
     ("Validating_Annex_H : " & Boolean'Image (Impdef.Validating_Annex_H));

   Comment ("Minimum_Task_Switch: ");
   Dio.Put (Impdef.Minimum_Task_Switch);
   New_Line;

   Comment ("Switch_To_New_Task: ");
   Dio.Put (Impdef.Switch_To_New_Task);
   New_Line;

   Comment ("Clear_Ready_Queue: ");
   Dio.Put (Impdef.Clear_Ready_Queue);
   New_Line;

   Comment ("Delay_For_Time_Past: ");
   Dio.Put (Impdef.Delay_For_Time_Past);
   New_Line;

   Comment ("Time_Dependent_Reset: ");
   Dio.Put (Impdef.Time_Dependent_Reset);
   New_Line;

   Comment ("Delay_Per_Random_Test: ");
   Dio.Put (Impdef.Delay_Per_Random_Test);
   New_Line;

   Report.Comment ("Exceed_Time_Slice");
   Impdef.Exceed_Time_Slice;

   Report.Comment ("Non_State_String: " & Impdef.Non_State_String);

   Report.Comment ("External_Tag_Value: " & Impdef.External_Tag_Value);

   Report.Comment ("CD30005_1_Foreign_Address: present");
   if Impdef.Cd30005_1_Foreign_Address'Alignment = 0 then
      null;
   end if;

   Report.Comment
     ("CD30005_1_External_Name: " & Impdef.Cd30005_1_External_Name);

   Report.Comment
     ("Max_Default_Alignment: " &
      Natural'Image (Impdef.Max_Default_Alignment));

   Report.Comment
     ("Max_Linker_Alignment: " & Natural'Image (Impdef.Max_Linker_Alignment));

   Report.Comment ("CXB30130_External_Name: " & Impdef.Cxb30130_External_Name);

   Report.Comment ("CXB30131_External_Name: " & Impdef.Cxb30131_External_Name);

   Report.Result;

end Cz00004;
