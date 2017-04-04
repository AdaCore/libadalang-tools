-- CE3704B.ADA

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
--     CHECK THAT INTEGER_IO GET RAISES MODE_ERROR FOR FILES OF MODE
--     OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/04/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  CORRECTED EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3704b is
   Incomplete : exception;

begin

   Test
     ("CE3704B",
      "CHECK THAT INTEGER_IO GET RAISES " &
      "MODE_ERROR FOR FILES OF MODE OUT_FILE");

   declare
      Ft : File_Type;
      type Int is new Integer range 1 .. 10;
      package Iio is new Integer_Io (Int);
      use Iio;
      X : Int := 10;
   begin

      begin
         Create (Ft, Out_File);
         Put (Ft, '3');
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " &
               "FOR TEMP FILE WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      begin
         Get (Ft, X);
         Failed ("MODE_ERROR NOT RAISED - FILE");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FILE");
      end;

      begin
         Get (Standard_Output, X);
         Failed ("MODE_ERROR NOT RAISED - STANDARD_OUTPUT");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - STANDARD_OUTPUT");
      end;

      begin
         Get (Current_Output, X);
         Failed ("MODE_ERROR NOT RAISED - CURRENT_OUTPUT");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CURRENT_OUTPUT");
      end;

      Close (Ft);
   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3704b;
