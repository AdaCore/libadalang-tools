-- CE3406D.ADA

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
--     CHECK THAT SKIP_PAGE OPERATES ON THE CURRENT DEFAULT INPUT
--     FILE WHEN NO FILE IS SPECIFIED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     JBG 01/26/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/24/87  CREATED NON-TEMPORARY FILE, REMOVED DEPENDENCE
--                   ON RESET, AND CHECKED CHARACTER READ IN.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3406d is

   Incomplete : exception;
   File      : File_Type;
   Item_Char : Character;
   Two       : Positive_Count := Positive_Count (Ident_Int (2));
   Three     : Positive_Count := Positive_Count (Ident_Int (3));

begin

   Test
     ("CE3406D",
      "CHECK THAT SKIP_PAGE OPERATES ON THE CURRENT " &
      "DEFAULT INPUT FILE WHEN NO FILE IS SPECIFIED");

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

   Put (File, "ABC");
   New_Page (File);
   Put (File, "DEF");

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
   end;

   Set_Input (File);

   Skip_Page;

   Get (File, Item_Char);
   if Item_Char /= 'D' then
      Failed ("INCORRECT VALUE READ FROM FILE");
   end if;

   if Page (Current_Input) /= Two then
      Failed ("SKIP_PAGE NOT APPLIED TO CURRENT_INPUT");
   end if;

   Skip_Page (File);

   if Page (Current_Input) /= Three then
      Failed ("SKIP_PAGE NOT APPLIED TO CURRENT_INPUT");
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

end Ce3406d;
