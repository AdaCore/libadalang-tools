-- CE3806H.ADA

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
--     CHECK THAT FIXED_IO PUT RAISES LAYOUT_ERROR WHEN THE NUMBER OF
--     CHARACTERS TO BE OUTPUT EXCEEDS THE MAXIMUM LINE LENGTH.  CHECK
--     THAT IT IS NOT RAISED, BUT RATHER NEW_LINE IS CALLED, WHEN THE
--     NUMBER DOES NOT EXCEED THE MAX, BUT WHEN ADDED TO THE CURRENT
--     COLUMN NUMBER, THE TOTAL EXCEEDS THE MAX.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 09/15/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3806h is

begin

   Test
     ("CE3806H", "CHECK THAT FIXED_IO PUT RAISES " & "LAYOUT_ERROR CORRECTLY");

   declare
      Ft : File_Type;
      type Fx is delta 0.01 range -200.0 .. 200.0;
      package Fxio is new Fixed_Io (Fx);
      use Fxio;
      Incomplete : exception;
      X : Fx := 126.5;
      Y : Fx := -134.0;
      Z : Fx := 120.0;

   begin

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      Set_Line_Length (Ft, 4);

      begin
         Put (Ft, X, Fore => 3, Aft => 1);
         Failed ("LAYOUT_ERROR NOT RAISED - FIXED");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED");
      end;

      Set_Line_Length (Ft, 7);

      begin
         Put (Ft, Y, Fore => 3, Aft => 2);
      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED SECOND PUT - " & "FIXED");
         when others =>
            Failed ("SOME EXCEPTION RAISED SECOND PUT - " & "FIXED");
      end;

      begin
         Put (Ft, Z, Fore => 4, Aft => 2);
         if Line (Ft) /= 2 then
            Failed ("NEW_LINE NOT CALLED - FIXED");
         end if;
      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED THIRD PUT - " & "FIXED");
         when others =>
            Failed ("EXCEPTION RAISED THIRD PUT - FIXED");
      end;

      begin
         Put (Ft, "Y");
         Put (Ft, Z, Fore => 3, Aft => 0);
         New_Line (Ft);
         Put (Ft, "Z");
         Put (Ft, Y, Fore => 3, Aft => 2);
      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED LAST PUT - " & "FIXED");
         when others =>
            Failed ("EXCEPTION RAISED LAST PUT - FIXED ");
      end;

      Check_File (Ft, "-134.00# 120.00#Y120.0#Z#-134.00#@%");

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

end Ce3806h;
