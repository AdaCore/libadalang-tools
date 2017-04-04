-- CE3906E.ADA

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
-- HISTORY:
--     CHECK THAT PUT FOR ENUMERATION TYPES RAISES LAYOUT_ERROR WHEN
--     THE NUMBER OF CHARACTERS TO BE OUTPUT EXCEEDS THE MAXIMUM LINE
--     LENGTH. CHECK THAT LAYOUT_ERROR IS NOT RAISED WHEN THE NUMBER
--     OF CHARACTERS TO BE OUTPUT DOES NOT EXCEED THE MAXIMUM LINE
--     LENGTH, BUT WHEN ADDED TO THE CURRENT COLUMN NUMBER, THE TOTAL
--     EXCEEDS THE MAXIMUM LINE LENGTH.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMETATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/11/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/18/87  CORRECTED EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3906e is
   Incomplete : exception;

begin

   Test
     ("CE3906E",
      "CHECK THAT ENUMERATION_IO PUT RAISES " & "LAYOUT_ERROR CORRECTLY");

   declare
      Ft : File_Type;
      type Color is (Red, Blu, Yellow, Orange, Rd);
      package Color_Io is new Enumeration_Io (Color);
      use Color_Io;
      Crayon : Color := Orange;
   begin

      begin
         Create (Ft);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT " &
               "CREATE FOR TEMP FILES WITH " &
               "OUT_FILE MODE - 1");
            raise Incomplete;
      end;

      Set_Line_Length (Ft, 5);

      begin
         Put (Ft, Crayon);
         Failed ("LAYOUT_ERROR NOT RAISED");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED");
      end;

      Put (Ft, Red);

      Put (Ft, Blu);
      if Line (Ft) /= 2 then
         Failed ("PUT DID NOT CAUSE NEW_LINE EFFECT");
      end if;

      Put (Ft, Rd);

      Check_File (Ft, "RED#" & "BLURD#@%");

      Close (Ft);

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3906e;
