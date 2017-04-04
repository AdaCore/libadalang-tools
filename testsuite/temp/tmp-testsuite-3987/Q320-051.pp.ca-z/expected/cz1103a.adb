-- CZ1103A.ADA
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
--     CHECK THAT THE PROCEDURE CHECK_FILE WORKS CORRECTLY, IN
--     PARTICULAR, THAT IT WILL REPORT INCORRECT FILE CONTENTS
--     AS TEST FAILURES.

--     THIS TEST INTENTIONALLY CONTAINS MISMATCHES BETWEEN FILE
--     CONTENTS AND THE 'CONTENTS' STRING PARAMETER OF PROCEDURE
--     CHECK_FILE.

-- PASS/FAIL CRITERIA:
--     IF AN IMPLEMENTATION SUPPORTS EXTERNAL FILES, IT PASSES THIS TEST
--     IF TEST EXECUTION REPORTS THE FOLLOWING FOUR FAILURES, REPORTS AN
--     INTERMEDIATE "FAILED" RESULT, REPORTS A FINAL "TENTATIVELY PASSED"
--     RESULT, AND REPORTS NO OTHER FAILURES.
--        * CZ1103A FROM CHECK_FILE: END OF LINE EXPECTED - E
--                     ENCOUNTERED.
--        * CZ1103A FROM CHECK_FILE: END_OF_PAGE NOT WHERE EXPECTED.
--        * CZ1103A FROM CHECK_FILE: END_OF_FILE NOT WHERE EXPECTED.
--        * CZ1103A FROM CHECK_FILE: FILE DOES NOT CONTAIN CORRECT
--                      OUTPUT - EXPECTED C - GOT I.
--
--     IF AN IMPLEMENTATION DOES NOT SUPPORT EXTERNAL FILES, IT PASSES THIS
--     TEST IF TEST EXECUTION REPORTS NINE FAILURES FOR INCOMPLETE SUBTESTS
--     SIMILAR TO THE SAMPLE BELOW, REPORTS AN INTERMEDIATE "FAILED" RESULT,
--     REPORTS A FINAL "TENTATIVELY PASSED" RESULT, AND REPORTS NO OTHER
--     FAILURES.
--        * CZ1103A TEST WITH EMPTY FILE INCOMPLETE.

-- HISTORY:
--     SPS 12/09/82  CREATED ORIGINAL TEST.
--     JRK 11/18/85  ADDED COMMENTS ABOUT PASS/FAIL CRITERIA.
--     JET 01/13/88  UPDATED HEADER FORMAT, ADDED FINAL CALL TO
--                   SPECIAL_ACTION.
--     PWB 06/24/88  CORRECTED PASS/FAIL CRITERIA TO INDICATE THE
--                   EXPECTED RESULT (TENTATIVELY PASSED).
--     RLB 03/20/00  CORRECTED PASS/FAIL CRITERIA TO REFLECT PROPER RESULT
--                   FOR AN IMPLEMENTATION THAT DOES NOT SUPPORT EXTERNAL FILES.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Cz1103a is

   Null_File                    : File_Type;
   File_With_Blank_Lines        : File_Type;
   File_With_Blank_Pages        : File_Type;
   File_With_Trailing_Blanks    : File_Type;
   File_Without_Trailing_Blanks : File_Type;
   File_With_End_Of_Line_Error  : File_Type;
   File_With_End_Of_Page_Error  : File_Type;
   File_With_End_Of_File_Error  : File_Type;
   File_With_Data_Error         : File_Type;

begin

   Test ("CZ1103A", "CHECK THAT PROCEDURE CHECK_FILE WORKS");

-- THIS SECTION TESTS CHECK_FILE WITH AN EMPTY FILE.

   begin
      Comment ("BEGIN TEST WITH AN EMPTY FILE");
      Create (Null_File, Out_File);
      Check_File (Null_File, "#@%");
      Close (Null_File);
   exception
      when others =>
         Failed ("TEST WITH EMPTY FILE INCOMPLETE");

   end;

-- THIS SECTION TESTS CHECK_FILE WITH A FILE WITH BLANK LINES.

   begin
      Comment ("BEGIN TEST WITH A FILE WITH BLANK LINES");
      Create (File_With_Blank_Lines, Out_File);
      New_Line (File_With_Blank_Lines, 20);
      Check_File (File_With_Blank_Lines, "####################@%");
      Close (File_With_Blank_Lines);
   exception
      when others =>
         Failed ("TEST WITH FILE WITH BLANK LINES INCOMPLETE");
   end;

-- THIS SECTION TESTS CHECK_FILE WITH A FILE WITH BLANK LINES AND PAGES.

   begin
      Comment ("BEGIN TEST WITH A FILE WITH BLANK LINES " & "AND PAGES");
      Create (File_With_Blank_Pages, Out_File);
      New_Line (File_With_Blank_Pages, 3);
      New_Page (File_With_Blank_Pages);
      New_Line (File_With_Blank_Pages, 2);
      New_Page (File_With_Blank_Pages);
      New_Page (File_With_Blank_Pages);
      Check_File (File_With_Blank_Pages, "###@##@#@%");
      Close (File_With_Blank_Pages);
   exception
      when others =>
         Failed ("TEST WITH FILE WITH BLANK PAGES INCOMPLETE");
   end;

-- THIS SECTION TESTS CHECK_FILE WITH A FILE WITH TRAILING BLANKS.

   begin
      Comment ("BEGIN TEST WITH A FILE WITH TRAILING BLANKS");
      Create (File_With_Trailing_Blanks, Out_File);
      for I in 1 .. 3 loop
         Put_Line (File_With_Trailing_Blanks, "LINE WITH TRAILING BLANKS    ");
      end loop;
      Check_File
        (File_With_Trailing_Blanks,
         "LINE WITH TRAILING" &
         " BLANKS#LINE WITH TRAILING BLANKS#LINE" &
         " WITH TRAILING BLANKS#@%");
      Close (File_With_Trailing_Blanks);
   exception
      when others =>
         Failed ("TEST WITH FILE WITH TRAILING BLANKS " & "INCOMPLETE");
   end;

-- THIS SECTION TESTS CHECK_FILE WITH A FILE WITHOUT TRAILING BLANKS.

   begin
      Comment ("BEGIN TEST WITH A FILE WITHOUT TRAILING BLANKS");
      Create (File_Without_Trailing_Blanks, Out_File);
      for I in 1 .. 3 loop
         Put_Line
           (File_Without_Trailing_Blanks,
            "LINE WITHOUT TRAILING BLANKS");
      end loop;
      Check_File
        (File_Without_Trailing_Blanks,
         "LINE WITHOUT " &
         "TRAILING BLANKS#LINE WITHOUT TRAILING BLANKS#" &
         "LINE WITHOUT TRAILING BLANKS#@%");
      Close (File_Without_Trailing_Blanks);
   exception
      when others =>
         Failed ("TEST WITH FILE WITHOUT TRAILING BLANKS " & "INCOMPLETE");
   end;

-- THIS SECTION TESTS CHECK_FILE WITH A FILE WITH AN END OF LINE ERROR.

   begin
      Comment ("BEGIN TEST WITH A FILE WITH AN END OF LINE ERROR");
      Create (File_With_End_Of_Line_Error, Out_File);
      Put_Line
        (File_With_End_Of_Line_Error,
         "THIS LINE WILL " & "CONTAIN AN END OF LINE IN THE WRONG PLACE");
      Check_File
        (File_With_End_Of_Line_Error,
         "THIS LINE WILL " & "CONTAIN AN # IN THE WRONG PLACE#@%");
      Close (File_With_End_Of_Line_Error);
   exception
      when others =>
         Failed ("TEST WITH END_OF_LINE ERROR INCOMPLETE");
   end;

-- THIS SECTION TESTS CHECK_FILE WITH A FILE WITH AN END OF PAGE ERROR.

   begin
      Comment ("BEGIN TEST WITH FILE WITH END OF PAGE ERROR");
      Create (File_With_End_Of_Page_Error, Out_File);
      Put_Line
        (File_With_End_Of_Page_Error,
         "THIS LINE WILL " & "CONTAIN AN END OF PAGE IN THE WRONG PLACE");
      Check_File
        (File_With_End_Of_Page_Error,
         "THIS LINE WILL " & "CONTAIN AN @ IN THE WRONG PLACE#@%");
      Close (File_With_End_Of_Page_Error);
   exception
      when others =>
         Failed ("TEST WITH END_OF_PAGE ERROR INCOMPLETE");
   end;

-- THIS SECTION TESTS CHECK_FILE WITH A FILE WITH AN END OF FILE ERROR.

   begin
      Comment ("BEGIN TEST WITH FILE WITH END OF FILE ERROR");
      Create (File_With_End_Of_File_Error, Out_File);
      Put_Line
        (File_With_End_Of_File_Error,
         "THIS LINE WILL " & "CONTAIN AN END OF FILE IN THE WRONG PLACE");
      Check_File
        (File_With_End_Of_File_Error,
         "THIS LINE WILL " & "CONTAIN AN % IN THE WRONG PLACE#@%");
      Close (File_With_End_Of_File_Error);
   exception
      when others =>
         Failed ("TEST WITH END_OF_FILE ERROR INCOMPLETE");
   end;

-- THIS SECTION TESTS CHECK_FILE WITH A FILE WITH INCORRECT DATA.

   begin
      Comment ("BEGIN TEST WITH FILE WITH INCORRECT DATA");
      Create (File_With_Data_Error, Out_File);
      Put_Line (File_With_Data_Error, "LINE WITH INCORRECT " & "DATA");
      Check_File (File_With_Data_Error, "LINE WITH CORRECT " & "DATA#@%");
      Close (File_With_Data_Error);
   exception
      when others =>
         Failed ("TEST WITH INCORRECT DATA INCOMPLETE");
   end;

   Result;

   Test ("CZ1103A", "THE LINE ABOVE SHOULD REPORT FAILURE");
   Special_Action ("COMPARE THIS OUTPUT TO THE EXPECTED RESULT");
   Result;

end Cz1103a;
