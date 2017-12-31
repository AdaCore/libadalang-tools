-- CE3409D.ADA

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
--     CHECK THAT, FOR FILES OF MODE IN_FILE, SET_COL READS UNTIL A
--     LINE FOUND HAVING A CHARACTER AT THE SPECIFIED COLUMN, SKIPPING
--     LINE AND PAGE TERMINATORS AS NECESSARY.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JBG 01/27/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/31/87  REMOVED DEPENDENCE ON REST, REMOVED UNNECESSARY
--                   CODE, CHECKED FOR USE_ERROR ON DELETE, AND ADDED
--                   NEW CASES FOR SET_COL.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3409d is

   Incomplete : exception;
   File      : File_Type;
   Four      : Positive_Count := Positive_Count (Ident_Int (4));
   Item_Char : Character;

begin

   Test
     ("CE3409D",
      "CHECK THAT SET_COL SKIPS LINE AND PAGE " &
      "TERMINATORS WHEN NECESSARY");

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

   Put (File, "ABC");
   New_Line (File);
   Put (File, "DEFGHI");
   New_Page (File);
   Put (File, "XYZ");
   New_Page (File);
   Put (File, "IJKL");

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT OPEN WITH " & "MODE IN_FILE");
         raise Incomplete;
   end;

   Set_Col (File, Four);
   Get (File, Item_Char);

   if Item_Char = ' ' then
      begin
         Comment ("FILE PADS LINES WITH SPACES");

         Set_Col (File, Four);
         Get (File, Item_Char);
         if Item_Char /= 'G' then
            Failed ("INCORRECT VALUE FROM SET_COL - 1");
         end if;

         Set_Col (File, Four);
         Get (File, Item_Char);
         if Item_Char /= ' ' then
            Failed ("LINES SHOULD STILL BE PADDED WITH BLANKS");
         end if;
      end;

   elsif Item_Char /= 'G' then
      Failed
        ("SET_COL DOESN'T SKIP LINE MARKS; " & "ACTUALLY READ '" & Item_Char &
         "'");
   else
      begin
         Set_Col (File, Four);
         Get (File, Item_Char);

         if Item_Char /= 'L' then
            Failed
              ("SET_COL DOESN'T SKIP PAGE MARKS; " & "ACTUALLY READ '" &
               Item_Char & "'");
         end if;
      end;
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

end Ce3409d;
