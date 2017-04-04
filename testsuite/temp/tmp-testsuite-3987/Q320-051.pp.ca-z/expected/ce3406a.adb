-- CE3406A.ADA

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
--     CHECK THAT SKIP_PAGE READS AND DISCARDS CHARACTERS AND LINE
--     TERMINATORS UNTIL A PAGE TERMINATOR IS READ, ADDS ONE TO THE
--     CURRENT PAGE NUMBER, AND SETS THE CURRENT COLUMN NUMBER AND LINE
--     NUMBER TO ONE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/17/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/24/87  CREATED NON-TEMPORARY FILE, REMOVED DEPENDENCE
--                   ON RESET, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3406a is

   Incomplete : exception;
   File      : File_Type;
   Char_X    : Character      := ('X');
   Item_Char : Character;
   One       : Positive_Count := Positive_Count (Ident_Int (1));
   Two       : Positive_Count := Positive_Count (Ident_Int (2));
   Three     : Positive_Count := Positive_Count (Ident_Int (3));

begin

   Test
     ("CE3406A",
      "CHECK THAT SKIP_LINE READS AND " & "SETS PAGE AND COLUMN CORRECTLY");

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
         raise Incomplete;
   end;

   Put (File, "CDE");
   New_Line (File);
   Put (File, "FGHI");
   New_Line (File);
   Put (File, "JK");
   New_Page (File);
   New_Page (File);
   Put (File, Char_X);

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
   end;

   if (Line (File) /= One) then
      Failed ("LINE NUMBER NOT EQUAL TO ONE");
   end if;

   if (Page (File) /= One) then
      Failed ("PAGE NUMBER NOT EQUAL TO ONE");
   end if;

   Get (File, Item_Char);

   if Item_Char /= 'C' then
      Failed ("INCORRECT VALUE READ FROM FILE - 1");
   end if;

   Skip_Page (File);

   if Col (File) /= One then
      Failed ("COLUMN NOT SET TO ONE - 1");
   end if;

   if Line (File) /= One then
      Failed ("LINE NOT SET TO ONE - 1");
   end if;

   if Page (File) /= Two then
      Failed ("PAGE NOT SET TO TWO");
   end if;

   Skip_Page (File);

   if Col (File) /= One then
      Failed ("COLUMN NOT SET TO ONE - 2");
   end if;

   if Line (File) /= One then
      Failed ("LINE NOT SET TO ONE - 2");
   end if;

   if Page (File) /= Three then
      Failed ("PAGE NOT SET TO THREE");
   end if;

   Get (File, Item_Char);
   if Item_Char /= 'X' then
      Failed ("INCORRECT VALUE READ FROM FILE - 2");
   end if;

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

end Ce3406a;
