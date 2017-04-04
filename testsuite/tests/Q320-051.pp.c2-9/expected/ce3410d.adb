-- CE3410D.ADA

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
--     CHECK THAT, FOR FILES OF MODE IN_FILE, SET_LINE READS UNTIL A
--     PAGE IS FOUND HAVING A LINE AT THE SPECIFIED POSITION, SKIPPING
--     LINE AND PAGE TERMINATORS AS NECESSARY.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JBG 01/27/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/01/87  REMOVED DEPENDENCE ON RESET AND CHECKED FOR
--                   USE_ERROR ON DELETE.
--     GJD 11/15/95  FIXED ADA 95 INCOMPATIBLE USE OF CHARACTER LITERALS.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3410d is

   Incomplete : exception;
   File      : File_Type;
   Four      : Positive_Count := Positive_Count (Ident_Int (4));
   Item_Char : Character;

begin

   Test
     ("CE3410D",
      "CHECK THAT SET_LINE SKIPS PAGE " & "TERMINATORS WHEN NECESSARY");

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT CREATE WITH " & "MODE OUT_FILE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON TEXT CREATE " & "WITH MODE OUT_FILE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
         raise Incomplete;
   end;

   for I in Character range 'A' .. 'C' loop
      Put (File, I);
      New_Line (File);
   end loop;

   New_Page (File);

   for I in Character range 'D' .. 'H'   -- 5 LINES
   loop
      Put (File, I);
      New_Line (File);
   end loop;

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
   end;

   Set_Line (File, Four);
   Get (File, Item_Char);

   if Item_Char /= 'G' then
      Failed
        ("SET_LINE DOESN'T SKIP PAGE MARKS; " &
         "ACTUALLY READ '" &
         Item_Char &
         "'");
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

end Ce3410d;
