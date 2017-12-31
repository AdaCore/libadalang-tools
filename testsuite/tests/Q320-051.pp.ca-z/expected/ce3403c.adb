-- CE3403C.ADA

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
--     CHECK THAT SKIP_LINE SETS THE CURRENT COLUMN NUMBER TO ONE,
--     AND THAT IT IS PERFORMED SPACING TIMES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/08/87  REVISED EXCEPTION HANDLING, REMOVED
--                   DEPENDENCE ON RESET, AND ADDED NEW CASES.
--     GJD 11/15/95  FIXED ADA 95 INCOMPATIBLE USE OF CHARACTER LITERALS.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3403c is

   Incomplete : exception;
   File  : File_Type;
   One   : Positive_Count := Positive_Count (Ident_Int (1));
   Spac3 : Positive_Count := Positive_Count (Ident_Int (3));
   Four  : Positive_Count := Positive_Count (Ident_Int (4));
   Ch    : Character;

begin

   Test
     ("CE3403C",
      "CHECK THAT SKIP_LINE SETS THE CURRENT " &
      "COLUMN NUMBER TO ONE, AND THAT IT IS " & "PERFORMED SPACING TIMES");

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

   for I in Character range 'A' .. 'E' loop
      for J in 1 .. 3 loop
         Put (File, I);
      end loop;
      New_Line (File);
   end loop;

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED; TEXT OPEN " & "FOR IN_FILE MODE");
         raise Incomplete;
   end;

   if Col (File) /= One then
      Failed ("COLUMN NOT SET TO ONE");
   end if;

   Get (File, Ch);

   if Ch /= 'A' then
      Failed ("INCORRECT VALUE READ - 1");
   end if;

   Skip_Line (File, Spac3);
   Get (File, Ch);

   if Ch /= 'D' then
      Failed ("INCORRECT VALUE READ - 2");
   end if;

   if Line (File) /= Four then
      Failed ("NOT PERFORMED SPACING TIMES");
   end if;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3403c;
