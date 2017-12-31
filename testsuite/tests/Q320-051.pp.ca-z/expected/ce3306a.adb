-- CE3306A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE VALUE OF 'TO' IS
--     NEGATIVE OR GREATER THAN COUNT'LAST WHEN COUNT'LAST IS LESS THAN
--     COUNT'BASE'LAST.

-- HISTORY:
--     JET 08/17/88  CREATED ORIGINAL TEST.
--     PWN 10/27/95  REMOVED CONSTRAINT CHECK THAT NOW HAPPENS AT
--                   COMPILE TIME.

with Report;  use Report;
with Text_Io; use Text_Io;
procedure Ce3306a is

begin
   Test
     ("CE3306A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE " &
      "VALUE OF 'TO' IS NEGATIVE OR GREATER THAN " &
      "COUNT'LAST WHEN COUNT'LAST IS LESS THAN " & "COUNT'BASE'LAST");

   begin
      Set_Line_Length (-1);
      Failed ("NO EXCEPTION FOR SET_LINE_LENGTH(-1)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION FOR SET_LINE_LENGTH(-1)");
   end;

   begin
      Set_Page_Length (Count (Ident_Int (-1)));
      Failed ("NO EXCEPTION FOR SET_PAGE_LENGTH(-1)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION FOR SET_PAGE_LENGTH(-1)");
   end;

   if Count'Last < Count'Base'Last then
      begin
         Set_Line_Length (Count'Last + Count (Ident_Int (1)));
         Failed ("NO EXCEPTION FOR SET_LINE_LENGTH(COUNT'LAST+1)");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("UNEXPECTED EXCEPTION FOR SET_LINE_LENGTH" & "(COUNT'LAST+1)");
      end;

   else
      Comment ("COUNT'LAST IS EQUAL TO COUNT'BASE'LAST");
   end if;

   Result;
end Ce3306a;
