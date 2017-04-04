-- CE3403F.ADA

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
--     CHECK THAT SKIP_LINE RAISES END_ERROR IF AN ATTEMPT IS
--     MADE TO SKIP A FILE TERMINATOR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 11/11/82
--     SPS 12/14/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  REVISED TEST TO USE A FILE NAME, REMOVED
--                   DEPENDENCE ON RESET, AND ADDED ATTEMPT TO
--                   DELETE THE FILE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3403f is

   Incomplete : exception;
   File : File_Type;
   Char : Character      := ('C');
   One  : Positive_Count := Positive_Count (Ident_Int (1));
   Two  : Positive_Count := Positive_Count (Ident_Int (2));

begin
   Test
     ("CE3403F",
      "CHECK THAT SKIP_LINE RAISES END_ERROR " &
      "IF AN ATTEMPT IS MADE TO SKIP A FILE " &
      "TERMINATOR");

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

   for I in 1 .. 3 loop
      Put (File, Char);
   end loop;

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED; TEXT OPEN " & "FOR IN_FILE MODE");
         raise Incomplete;
   end;

   begin
      Get (File, Char);
      if Char /= 'C' then
         Failed ("INCORRECT VALUE READ");
      end if;

      Skip_Line (File);
      Skip_Line (File);
      Failed ("END_ERROR NOT RAISED - 1");
   exception
      when End_Error =>

         if Col (File) /= One then
            Failed ("COL NOT RESET CORRECTLY");
         end if;

         if not End_Of_File (File) then
            Failed ("NOT POSITIONED AT END OF FILE");
         end if;

         if Page (File) /= Two then
            Failed ("PAGE NOT INCREMENTED");
         end if;

         if Line (File) /= One then
            Failed ("LINE NOT RESET CORRECTLY");
         end if;

         if not End_Of_Line (File) then
            Failed ("EOL FALSE AT FILE TERMINATOR");
         end if;

         if not End_Of_Page (File) then
            Failed ("EOP FALSE AT FILE TERMINATOR");
         end if;

         begin
            Skip_Line (File);
            Failed ("END_ERROR NOT RAISED - 2");
         exception
            when End_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 1");
         end;

      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 2");
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

end Ce3403f;
