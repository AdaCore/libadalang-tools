-- CE3706F.ADA

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
--     CHECK THAT INTEGER_IO PUT RAISES LAYOUT_ERROR WHEN THE NUMBER OF
--     CHARACTERS TO BE OUTPUT EXCEEDS THE MAXIMUM LINE LENGTH.  CHECK
--     THAT IT IS NOT RAISED WHEN THE NUMBER OF CHARACTERS TO BE OUTPUT
--     ADDED TO THE CURRENT COLUMN NUMBER EXCEEDS THE MAXIMUM LINE
--     LENGTH.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF TEMPORARY TEXT FILES WITH OUT_FILE MODE.

-- HISTORY:
--     SPS 10/05/82
--     VKG 01/14/83
--     SPS 02/18/83
--     JBG 08/30/83
--     EG  05/22/85
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/10/87  REMOVED UNNECESSARY CODE, CORRECTED EXCEPTION
--                   HANDLING, AND ADDED CASE USING WIDTH OF FIVE.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3706f is

begin

   Test ("CE3706F", "CHECK THAT LAYOUT_ERROR IS RAISED CORRECTLY");

   declare
      Ft : File_Type;
      package Iio is new Integer_Io (Integer);
      use Iio;
      Incomplete : exception;
   begin

      begin
         Create (Ft);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " &
               "FOR TEMPORARY FILE WITH " &
               "OUT_FILE MODE");
            raise Incomplete;
      end;

      Set_Line_Length (Ft, 4);

      begin
         Put (Ft, 32_000, Width => 0);
         Failed ("LAYOUT_ERROR NOT RAISED - 1");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 1");
      end;

      begin
         Put (Ft, 32_000, Width => 5);
         Failed ("LAYOUT_ERROR NOT RAISED - 2");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 2");
      end;

      Put (Ft, 123, Width => 0);  -- "123"

      begin
         Put (Ft, 457, Width => 0);  -- "123#457"
         if Line (Ft) /= 2 then
            Failed ("OUTPUT INCORRECT");
         end if;
      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED INCORRECTLY");
      end;

      Check_File (Ft, "123#457#@%");

      Close (Ft);

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3706f;
