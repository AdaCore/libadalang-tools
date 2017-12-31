-- CE3606A.ADA

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
--     CHECK THAT PUT_LINE WILL OUTPUT A LINE TERMINATOR WHEN THE
--     STRING PARAMETER IS NULL.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEMPORARY TEXT FILES.

-- HISTORY:
--     SPS 09/02/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  REMOVED UNNECESSARY CODE AND CORRECTED
--                   EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;
procedure Ce3606a is
   Incomplete : exception;

begin

   Test ("CE3606A", "PUT_LINE PUTS LINE TERMINATOR WHEN STRING " & "IS NULL");

   declare
      Ft  : File_Type;
      Ns1 : String (1 .. 0);
      Ns2 : String (3 .. 1);
      Lc  : Positive_Count := 1;
   begin

      begin
         Create (Ft);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " &
               "FOR TEMP FILES WITH OUT_FILE " & "MODE");
            raise Incomplete;
      end;

      Put_Line (Ft, Ns1);
      if Line (Ft) /= Lc + 1 then
         Failed
           ("PUT_LINE OF NULL STRING 1; LINE " & "COUNT WAS" &
            Count'Image (Line (Ft)));
      end if;

      Put_Line (Ft, Ns2);
      if Line (Ft) /= Lc + 2 then
         Failed
           ("PUT_LINE OF NULL STRING 2; LINE " & "COUNT WAS" &
            Count'Image (Line (Ft)));
      end if;

      Check_File (Ft, "##@%");

      Close (Ft);
   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3606a;
