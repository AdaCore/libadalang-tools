-- CE3405A.ADA

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
--     CHECK THAT NEW_PAGE OUTPUTS A LINE TERMINATOR FOLLOWED BY A PAGE
--     TERMINATOR IF THE CURRENT LINE IS NOT AT COLUMN 1 OR IF THE
--     CURRENT PAGE IS AT LINE 1;  IF THE CURRENT LINE IS AT COLUMN 1,
--     OUTPUTS A PAGE TERMINATOR ONLY.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 09/02/82
--     JBG 01/18/83
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/23/87  ADDED A CASE WHICH CALLS NEW_LINE AND NEW_PAGE
--                   CONSECUTIVELY AND SEPARATED CASES INTO DIFFERENT
--                   IF STATEMENTS.  ADDED CHECK FOR USE_ERROR ON
--                   DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3405a is

   Incomplete : exception;
   File  : File_Type;
   One   : Positive_Count := Positive_Count (Ident_Int (1));
   Two   : Positive_Count := Positive_Count (Ident_Int (2));
   Three : Positive_Count := Positive_Count (Ident_Int (3));
   Four  : Positive_Count := Positive_Count (Ident_Int (4));
   Char  : Character      := ('C');

begin

   Test
     ("CE3405A",
      "CHECK THAT NEW_PAGE OUTPUTS A LINE TERMINATOR " &
      "FOLLOWED BY A PAGE TERMINATOR IF THE CURRENT " &
      "LINE IS NOT AT COLUMN 1 OR IF THE CURRENT " &
      "PAGE IS AT LINE 1;  IF THE CURRENT LINE IS AT " &
      "COLUMN 1, OUTPUTS A PAGE TERMINATOR ONLY");

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
   end;

   New_Page (File);
   New_Page (File);                   -- CURRENT PAGE TERMINATED
   if Page (File) /= Three then
      Failed ("INITIAL PAGE COUNT INCORRECT");
   end if;

   Set_Line_Length (File, Three);
   Put (File, Char);
   New_Line (File);

   if Line (File) /= Two then
      Failed ("INCORRECT LINE NUMBER - 1");
   end if;

   if Page (File) /= Three then
      Failed ("INCORRECT PAGE NUMBER - 2");
   end if;

   New_Page (File);             -- CURRENT LINE TERMINATED (B)
   if Line (File) /= One then
      Failed ("LINE NUMBER NOT INCREMENTED");
   end if;
   if Page (File) /= Four then
      Failed ("PAGE NUMBER NOT INCREMENTED");
   end if;
   Put (File, Ident_Char ('E'));  -- CURRENT LINE NOT TERM (C)
   New_Page (File);
   New_Line (File);
   New_Page (File);

   Check_File (File, "#@#@C#@E#@#@%");

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

end Ce3405a;
