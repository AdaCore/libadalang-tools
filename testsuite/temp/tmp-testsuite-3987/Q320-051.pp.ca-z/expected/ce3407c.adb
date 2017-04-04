-- CE3407C.ADA

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
--     CHECK THAT THE FILE PARAMETER OF END_OF_PAGE IS OPTIONAL, AND
--     THAT THE FUNCTION IS THEN APPLIED TO THE CURRENT DEFAULT INPUT
--     FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/28/87  REMOVED DEPENDENCE ON RESET, ADDED MORE CASES FOR
--                   END_OF_PAGE, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3407c is

   Incomplete : exception;
   File_In   : File_Type;
   Char      : Character := 'C';
   Item_Char : Character;

begin

   Test
     ("CE3407C",
      "CHECK THAT THE FILE PARAMETER OF END_OF_PAGE " &
      "IS OPTIONAL, AND THAT THE FUNCTION IS THEN " &
      "APPLIED TO THE CURRENT DEFAULT INPUT FILE");

   begin
      Create (File_In, Out_File, Legal_File_Name);
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

   for I in 1 .. 3 loop
      Put (File_In, Char);
   end loop;
   New_Page (File_In);
   Put (File_In, 'D');

   Close (File_In);

   begin
      Open (File_In, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
   end;

   Set_Input (File_In);

   if End_Of_Page then
      Failed ("INCORRECT VALUE AT FIRST POSITION");
   end if;

   if End_Of_Page /= End_Of_Page (File_In) then
      Failed ("END OF PAGE DOES NOT OPERATE WITH DEFAULT FILE");
   end if;

   Get (Item_Char);
   Get (Item_Char);
   Get (Item_Char);

   if End_Of_Page /= True then
      Failed ("INCORRECT VALUE BEFORE PAGE TERMINATOR");
   end if;

   if End_Of_Page /= End_Of_Page (File_In) then
      Failed
        ("END_OF_PAGE WITHOUT PARAMETER DOES " &
         "NOT OPERATE ON THE DEFAULT INPUT FILE");
   end if;

   Get (Item_Char);

   if not (End_Of_Page) then
      Failed ("INCORRECT VALUE AT LAST POSITION");
   end if;

   begin
      Delete (File_In);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3407c;
