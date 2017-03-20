-- CE3403B.ADA

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
--     CHECK THAT THE SPACING PARAMETER OF SKIP_LINE IS OPTIONAL,
--     AND THAT THE DEFAULT VALUE IS ONE.
--     CHECK THAT THE FILE PARAMETER IS ALSO OPTIONAL, AND THAT THE
--     FUNCTION IS THEN APPLIED TO THE CURRENT DEFAULT INPUT FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 12/14/82
--     JBG 1/17/83
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/04/87  REVISED EXCEPTION HANDLERS, REMOVED
--                   DEPENDENCIES ON RESET, AND ADDED AN ATTEMPT
--                   TO DELETE FILE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3403b is

   Incomplete : exception;
   File      : File_Type;
   Spac, Two : Positive_Count := Positive_Count (Ident_Int (2));
   A         : Integer        := Character'Pos ('A');
   Ch        : Character;

begin

   Test ("CE3403B", "CHECK DEFAULT SPACING AND FILE " & "OF SKIP_LINE");

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
         Failed ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
         raise Incomplete;
   end;

   for I in 1 .. 3 loop          -- CREATES "BBB#CC#D##F#@%"
      for J in 1 .. 4 - I loop
         Put (File, Character'Val (A + I));
      end loop;
      New_Line (File);
   end loop;
   New_Line (File);
   Put (File, 'F');

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED; TEXT OPEN " & "FOR IN_FILE MODE");
         raise Incomplete;
   end;

   Get (File, Ch);
   if Ch /= Character'Val (A + 1) then
      Failed ("LINE CONTENT WRONG - 1");
   end if;

   Skip_Line (File);

   if Line (File) /= Two then
      Failed ("SPACING DEFAULT NOT ONE");
   end if;

   Get (File, Ch);
   if Ch /= Character'Val (A + 2) then
      Failed ("LINE CONTENT WRONG - 2");
   end if;

   Set_Input (File);
   Skip_Line (File);

   if Line (File) /= 3 then
      Failed ("SKIP_LINE DOES NOT OPERATE CORRECTLY ON " & "DEFAULT FILE");
   end if;

   Get (File, Ch);
   if Ch /= Character'Val (A + 3) then
      Failed ("LINE CONTENT WRONG - 3");
   end if;

   Skip_Line;

   if Line (File) /= 4 then
      Failed ("LINE COUNT NOT 4; WAS " & Count'Image (Line (File)));
   end if;

   Get (File, Ch);
   if Ch /= 'F' then
      Failed ("NOT RIGHT LINE");
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

end Ce3403b;
