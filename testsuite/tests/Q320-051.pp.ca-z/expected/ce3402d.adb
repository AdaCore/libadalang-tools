-- CE3402D.ADA

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
--     CHECK THAT NEW_LINE SETS THE CURRENT COLUMN NUMBER TO ONE,
--     AND NEW_LINE OUTPUTS LINE TERMINATORS WHEN THE SPACING IS
--     GREATER THAN ONE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH OUT_FILE MODE FOR TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/19/87  CHANGED FAILED MESSAGE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3402d is

   Incomplete : exception;
   File  : File_Type;
   One   : Positive_Count := Positive_Count (Ident_Int (1));
   Spac3 : Positive_Count := Positive_Count (Ident_Int (3));
   Four  : Positive_Count := Positive_Count (Ident_Int (4));

begin

   Test
     ("CE3402D",
      "CHECK THAT NEW_LINE SETS THE CURRENT " &
      "COLUMN NUMBER TO ONE, AND NEW_LINE OUTPUTS " &
      "TERMINATORS WHEN THE SPACING IS " & "GREATER THAN ONE");

   begin
      Create (File);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
         raise Incomplete;
   end;

   for I in 1 .. 5 loop
      Put (File, 'X');
   end loop;

   New_Line (File, Spac3);
   if Line (File) /= Four then
      Failed ("NEW_LINE DID NOT OUTPUT LINE TERMINATORS");
   end if;

   if Col (File) /= One then
      Failed ("COLUMN NOT SET TO ONE");
   end if;
   Close (File);

   Result;

exception
   when Incomplete =>
      Result;

end Ce3402d;
