-- CE3408A.ADA

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
--     CHECK THAT END_OF_FILE RETURNS TRUE ONLY IF POSITIONED BEFORE THE
--     FINAL PAGE TERMINATOR OR BEFORE THE FILE TERMINATOR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     JBG 01/26/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/31/87  REMOVED DEPENDENCE ON RESET, REMOVED UNNECESSARY
--                   CODE, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3408a is

   Incomplete : exception;
   Count     : Integer   := 0;
   File      : File_Type;
   Char      : Character := ('C');
   Item_Char : Character;

begin

   Test ("CE3408A", "CHECK THAT END_OF_FILE RETURNS " & "THE CORRECT VALUE");

-- CREATE & INITIALIZE OUTPUT FILE.

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT CREATE WITH " & "OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
         raise Incomplete;
   end;

   for I in 1 .. 6 loop
      Put (File, Char);
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

-- TEST WHEN POSITIONED TO BEGINNING OF FILE.

   if End_Of_File (File) then
      Failed ("INCORRECT VALUE AT FIRST POSITION - 1");
   end if;

   if End_Of_File (File) then
      Failed ("INCORRECT VALUE AT FIRST POSITION - 2");
   end if;

-- TEST WHEN POSITIONED BEFORE LAST CHARACTER IN FILE.

   for I in 1 .. 5 loop
      Get (File, Item_Char);
   end loop;

   if End_Of_File (File) then
      Failed ("INCORRECT VALUE BEFORE LAST CHARACTER");
   end if;

-- TEST WHEN AT END OF FILE.

   Get (File, Item_Char);
   if not End_Of_File (File) then
      Failed ("INCORRECT VALUE AT LAST POSITION");
   end if;

   Skip_Page (File);

   if not End_Of_File (File) then
      Failed ("INCORRECT VALUE BEFORE FILE TERMINATOR - 1");
   end if;

   if not End_Of_File (File) then
      Failed ("INCORRECT VALUE BEFORE FILE TERMINATOR - 2");
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

end Ce3408a;
