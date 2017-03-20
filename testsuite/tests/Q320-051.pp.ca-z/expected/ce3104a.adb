-- CE3104A.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE CURRENT COLUMN, LINE, AND PAGE NUMBERS OF
--     TEXT FILES ARE SET TO ONE AFTER A CREATE, OPEN, OR RESET.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/24/82
--     SPS 09/16/82
--     SPS 11/09/82
--     JBG 03/16/83
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/13/87  CHANGED FAILED MESSAGES AND ADDED SUBTEST
--                   EXCEPTION.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3104a is

   Incomplete, Subtest : exception;
   File, Ft : File_Type;
   One      : constant Positive_Count := Positive_Count (Ident_Int (1));
   Char     : Character;

begin

   Test
     ("CE3104A",
      "CHECK THAT COLUMN, LINE, AND " &
      "PAGE NUMBERS ARE ONE AFTER A " &
      "CREATE, OPEN, OR RESET");

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed
           ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE " &
            "WITH OUT_FILE MODE");
         raise Incomplete;
   end;

   if Col (File) /= One then
      Failed ("INCORRECT RESULTS FROM COLUMN AFTER CREATE");
   end if;
   if Line (File) /= One then
      Failed ("INCORRECT RESULTS FROM LINE AFTER CREATE");
   end if;
   if Page (File) /= One then
      Failed ("INCORRECT RESULTS FROM PAGE AFTER CREATE");
   end if;

   New_Page (File);
   New_Line (File);
   Put (File, "STRING");

   Close (File);

   begin
      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            raise Subtest;
      end;

      if Col (File) /= One then
         Failed ("INCORRECT RESULTS FROM COLUMN AFTER " & "OPEN - IN_FILE");
      end if;
      if Line (File) /= One then
         Failed ("INCORRECT RESULTS FROM LINE AFTER " & "OPEN - IN_FILE");
      end if;
      if Page (File) /= One then
         Failed ("INCORRECT RESULTS FROM PAGE AFTER " & "OPEN - IN_FILE");
      end if;

      Get (File, Char);   -- SETS PAGE, LINE, AND COL /= 1

      begin
         Reset (File);
      exception
         when Use_Error =>
            Close (File);
            raise Subtest;
      end;

      if Col (File) /= One then
         Failed ("INCORRECT RESULTS FROM COLUMN AFTER RESET");
      end if;
      if Line (File) /= One then
         Failed ("INCORRECT RESULTS FROM LINE AFTER RESET");
      end if;
      if Page (File) /= One then
         Failed ("INCORRECT RESULTS FROM PAGE AFTER RESET");
      end if;

      Get (File, Char);   -- CHANGES LINE, PAGE, COL; STILL IN_FILE

      begin
         Reset (File, Out_File);
      exception
         when Use_Error =>
            Close (File);
            raise Subtest;
      end;

      if Col (File) /= One then
         Failed ("INCORRECT RESULTS FROM COLUMN AFTER RESET " & "TO OUT_FILE");
      end if;
      if Line (File) /= One then
         Failed ("INCORRECT RESULTS FROM LINE AFTER RESET " & "TO OUT_FILE");
      end if;
      if Page (File) /= One then
         Failed ("INCORRECT RESULTS FROM PAGE AFTER RESET " & "TO OUT_FILE");
      end if;

      Close (File);

   exception
      when Subtest =>
         null;
   end;

   begin
      begin
         Open (File, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            raise Subtest;
      end;

      if Col (File) /= One then
         Failed ("INCORRECT RESULTS FROM COLUMN AFTER OPEN " & "TO OUT_FILE");
      end if;
      if Line (File) /= One then
         Failed ("INCORRECT RESULTS FROM LINE AFTER OPEN " & "TO OUT_FILE");
      end if;
      if Page (File) /= One then
         Failed ("INCORRECT RESULTS FROM PAGE AFTER OPEN " & "TO OUT_FILE");
      end if;

   exception
      when Subtest =>
         null;
   end;

   begin
      begin
         Create (Ft, In_File);
      exception
         when Use_Error =>
            raise Subtest;
      end;

      if Col (File) /= One then
         Failed ("INCORRECT RESULTS FROM COLUMN AFTER CREATE " & "IN IN_FILE");
      end if;
      if Line (File) /= One then
         Failed ("INCORRECT RESULTS FROM LINE AFTER CREATE " & "IN IN_FILE");
      end if;
      if Page (File) /= One then
         Failed ("INCORRECT RESULTS FROM PAGE AFTER CREATE " & "IN IN_FILE");
      end if;

      Close (Ft);

   exception
      when Subtest =>
         null;
   end;

   begin
      Delete (File);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;
end Ce3104a;
