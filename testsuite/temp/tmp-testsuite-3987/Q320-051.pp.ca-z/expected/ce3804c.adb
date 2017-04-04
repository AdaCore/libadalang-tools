-- CE3804C.ADA

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
--     CHECK THAT GET FOR FLOAT_IO RAISES MODE_ERROR WHEN THE
--     MODE IS NOT IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 09/07/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  SPLIT CASE FOR FIXED_IO INTO CE3804O.ADA
--                   AND CORRECTED EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3804c is
   Incomplete : exception;

begin

   Test
     ("CE3804C",
      "CHECK THAT GET FOR FLOAT_IO RAISES " &
      "MODE_ERROR WHEN THE MODE IS NOT IN_FILE");

   declare
      Ft2 : File_Type;
   begin

      begin
         Create (Ft2, Out_File);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " &
               "FOR TEMP FILES WITH OUT_FILE " &
               "MODE - 1");
            raise Incomplete;
      end;

      declare
         package Fl_Io is new Float_Io (Float);
         use Fl_Io;
         X : Float;
      begin

         begin
            Get (Ft2, X);
            Failed ("MODE_ERROR NOT RAISED - FLOAT " & "UN-NAMED FILE");
         exception
            when Mode_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - " & "FLOAT UN-NAMED FILE");
         end;

         begin
            Get (Standard_Output, X);
            Failed ("MODE_ERROR NOT RAISED - FLOAT " & "STANDARD_OUTPUT");
         exception
            when Mode_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - " & "FLOAT STANDARD_OUTPUT");
         end;

         begin
            Get (Current_Output, X);
            Failed ("MODE_ERROR NOT RAISED - FLOAT " & "CURRENT_OUTPUT");
         exception
            when Mode_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - " & "FLOAT CURRENT_OUTPUT");
         end;

      end;

      Close (Ft2);

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3804c;
