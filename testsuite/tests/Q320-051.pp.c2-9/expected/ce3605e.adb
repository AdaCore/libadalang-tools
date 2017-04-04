-- CE3605E.ADA

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
--     CHECK THAT PUT CAN BE CALLED WITH CHARACTER AND STRING
--     PARAMETERS.  CHECK THAT FILES OF MODE OUT_FILE ARE USED AND
--     THAT WHEN NO FILE IS SPECIFIED THE CURRENT DEFAULT OUTPUT FILE
--     IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF TEMPORARY TEXT FILES WITH OUT_FILE MODE.

-- HISTORY:
--     SPS 10/06/82
--     JBG 12/28/82
--     VKG 02/15/83
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  REMOVED UNNECESSARY CODE AND CHECKED FOR
--                   USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;
procedure Ce3605e is
   Incomplete : exception;

begin

   Test
     ("CE3605E",
      "CHECK THAT PUT FOR STRINGS AND CHARACTERS " &
      "OPERATES ON OUT_FILE FILES");

   declare
      Ft, File : File_Type;
      X        : Character;
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

      Create (File);

      Set_Output (File);

      Put (Ft, 'O');

      Put (Ft, "UTPUT STRING");

      Put ('X');

      Put ("UTPUT STRING");

-- CHECK OUTPUT

      Set_Output (Standard_Output);
      Comment ("CHECKING FT");
      Check_File (Ft, "OUTPUT STRING#@%");
      Comment ("CHECKING FILE");
      Check_File (File, "XUTPUT STRING#@%");

      Close (Ft);
      Close (File);

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3605e;
