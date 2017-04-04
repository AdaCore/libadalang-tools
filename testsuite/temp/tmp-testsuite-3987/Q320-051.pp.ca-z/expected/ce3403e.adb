-- CE3403E.ADA

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
--     CHECK THAT SKIP_LINE INCREMENTS THE CURRENT LINE NUMBER BY ONE
--     AND SETS THE CURRENT COLUMN NUMBER TO ONE IF THE LINE TERMINATOR
--     IS NOT FOLLOWED BY A PAGE TERMINATOR, AND THAT IT SETS BOTH THE
--     LINE AND COLUMN NUMBERS TO ONE AND INCREMENTS THE CURRENT PAGE
--     NUMBER BY ONE IF THE LINE TERMINATOR IS FOLLOWED BY A PAGE
--     TERMINATOR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/20/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  REVISED TEST TO USE A FILE NAME, REMOVED
--                   DEPENDENCE ON RESET, AND ATTEMPTED TO
--                   DELETE THE FILE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3403e is

   Incomplete : exception;
   File : File_Type;
   One  : Positive_Count := Positive_Count (Ident_Int (1));
   Two  : Positive_Count := Positive_Count (Ident_Int (2));
   Char : Character      := ('C');

begin

   Test
     ("CE3403E",
      "CHECK THAT SKIP_LINE SETS COLUMN, " &
      "LINE, AND PAGE NUMBERS CORRECTLY");

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

   Put (File, Char);
   New_Line (File);
   Put (File, Char);
   New_Page (File);
   Put (File, Char);

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED; TEXT OPEN " & "WITH IN_FILE MODE");
         raise Incomplete;
   end;

   if (Line (File) /= One) or (Page (File) /= One) then
      Failed ("INCORRECT LINE AND PAGE NUMBERS");
   else

-- LINE TERMINATOR NOT FOLLOWED BY PAGE TERMINATOR

      Get (File, Char);

      if Char /= 'C' then
         Failed ("INCORRECT VALUE READ - 1");
      end if;

      Skip_Line (File);
      if Line (File) /= Two then
         Failed ("FIRST SUBTEST - LINE NOT INCREMENTED");
      end if;
      if Col (File) /= One then
         Failed ("FIRST SUBTEST - COLUMN NOT SET TO ONE");
      end if;

-- LINE TERMINATOR FOLLOWED BY PAGE TERMINATOR

      Get (File, Char);

      if Char /= 'C' then
         Failed ("INCORRECT VALUE READ - 2");
      end if;

      Skip_Line (File);
      if Line (File) /= One then
         Failed ("SECOND SUBTEST - LINE NOT SET TO ONE");
      end if;
      if Col (File) /= One then
         Failed ("SECOND SUBTEST - COLUMN NOT SET TO ONE");
      end if;
      if Page (File) /= Two then
         Failed ("SECOND SUBTEST - PAGE NOT INCREMENTED");
      end if;
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

end Ce3403e;
