-- CE3804O.ADA

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
--     CHECK THAT GET FOR FIXED_IO RAISES MODE_ERROR WHEN THE
--     MODE IS NOT IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     DWC 09/14/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3804o is
   Incomplete : exception;

begin

   Test
     ("CE3804O",
      "CHECK THAT GET FOR FIXED_IO RAISES " &
      "MODE_ERROR WHEN THE MODE IS NOT IN_FILE");

   declare
      Ft : File_Type;
   begin
      begin
         Create (Ft, Out_File);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT " & "CREATE FOR TEMP FILES " &
               "WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      declare
         type Fixed is delta 0.25 range 1.0 .. 3.0;
         package Fx_Io is new Fixed_Io (Fixed);
         use Fx_Io;
         X : Fixed;
      begin

         begin
            Get (Ft, X);
            Failed ("MODE_ERROR NOT RAISED - FIXED " & "UN-NAMED FILE");
         exception
            when Mode_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - " & "FIXED UN-NAMED FILE");
         end;

         begin
            Get (Standard_Output, X);
            Failed ("MODE_ERROR NOT RAISED - FIXED " & "STANDARD_OUTPUT");
         exception
            when Mode_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - " & "FIXED STANDARD_OUTPUT");
         end;

         begin
            Get (Current_Output, X);
            Failed ("MODE_ERROR NOT RAISED - FIXED " & "CURRENT_OUTPUT");
         exception
            when Mode_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - " & "FIXED CURRENT_OUTPUT");
         end;

      end;

      begin
         Delete (Ft);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3804o;
