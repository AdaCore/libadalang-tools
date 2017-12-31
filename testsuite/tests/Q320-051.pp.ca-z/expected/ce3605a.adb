-- CE3605A.ADA

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
--     CHECK THAT PUT FOR CHARACTER AND STRING PARAMETERS DOES NOT
--     UPDATE THE LINE NUMBER WHEN THE LINE LENGTH IS UNBOUNDED,
--     ONLY THE COLUMN NUMBER.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF TEMPORARY TEXT FILES WITH OUT_FILE MODE.

-- HISTORY:
--     SPS 09/02/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  CORRECTED EXCEPTION HANDLING AND ADDED CHECKS
--                   FOR COLUMN NUMBER.
--     RJW 03/28/90  REVISED NUMERIC LITERALS USED IN LOOPS.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3605a is
   Incomplete : exception;

begin

   Test
     ("CE3605A",
      "CHECK THAT PUT FOR CHARACTER AND STRING " &
      "PARAMETERS DOES NOT UPDATE THE LINE NUMBER " &
      "WHEN THE LINE LENGTH IS UNBOUNDED, ONLY THE " & "COLUMN NUMBER");

   declare
      File1 : File_Type;
      Ln    : Positive_Count := 1;
   begin

      begin
         Create (File1);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " &
               "FOR TEMPORARY FILES WITH " & "OUT_FILE MODE");
            raise Incomplete;
      end;

      Ln := Line (File1);

      if Ln /= 1 then
         Failed ("CURRENT LINE NUMBER NOT INITIALLY ONE");
      end if;

      if Col (File1) /= 1 then
         Failed ("CURRENT COLUMN NUMBER NOT INITIALLY ONE");
      end if;

      for I in 1 .. Ident_Int (240) loop
         Put (File1, 'A');
      end loop;
      if Line (File1) /= Ln then
         Failed ("PUT ALTERED LINE NUMBER - CHARACTER");
      end if;

      if Col (File1) /= 241 then
         Failed ("COLUMN NUMBER NOT UPDATED CORRECTLY - 1");
      end if;

      New_Line (File1);
      Ln := Line (File1);

      for I in 1 .. Ident_Int (40) loop
         Put (File1, "STRING");
      end loop;
      if Ln /= Line (File1) then
         Failed ("PUT ALTERED LINE NUMBER - STRING");
      end if;

      if Col (File1) /= 241 then
         Failed ("COLUMN NUMBER NOT UPDATED CORRECTLY - 2");
      end if;

      Close (File1);

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3605a;
