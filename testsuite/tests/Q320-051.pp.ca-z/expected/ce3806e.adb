-- CE3806E.ADA

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
--     CHECK THAT FLOAT_IO PUT RAISE LAYOUT_ERROR WHEN THE NUMBER
--     OF CHARACTERS TO BE OUTPUT EXCEEDS THE MAXIMUM LINE LENGTH.
--     CHECK THAT IT IS NOT RAISED, BUT RATHER NEW_LINE IS CALLED,
--     WHEN THE NUMBER DOES NOT EXCEED THE MAX, BUT WHEN ADDED TO
--     THE CURRENT COLUMN NUMBER, THE TOTAL EXCEEDS THE MAX.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 10/07/82
--     SPS 12/14/82
--     VKG 01/13/83
--     SPS 02/18/83
--     JBG 08/30/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/14/87  REMOVED DEPENDENCE ON RESET AND CORRECTED
--                   EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3806e is

begin

   Test
     ("CE3806E", "CHECK THAT FLOAT_IO PUT RAISES " & "LAYOUT_ERROR CORRECTLY");

   declare
      type Fl is digits 3 range 100.0 .. 200.0;
      package Flio is new Float_Io (Fl);
      use Flio;
      X : Fl := 126.0;
      Y : Fl := 134.0;
      Z : Fl := 120.0;
      Incomplete : exception;
      Ft : File_Type;
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

      Set_Line_Length (Ft, 8);

      begin
         Put (Ft, X);        -- " 1.26E+02"
         Failed ("LAYOUT_ERROR NOT RAISED - FLOAT");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FLOAT");

      end;

      begin
         Put (Ft, Y, Fore => 1); -- "1.34E+02"
      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED SECOND PUT " & "- FLOAT");
         when others =>
            Failed ("EXCEPTION RAISED SECOND PUT - FLOAT");
      end;

      begin
         Put (Ft, Z, Fore => 1, Aft => 0); -- "1.2E+02"
         if Line (Ft) /= 2 then
            Failed ("NEW_LINE NOT CALLED - FLOAT");
         end if;
      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED THIRD " & "PUT - FLOAT");
         when others =>
            Failed ("EXCEPTION RAISED THIRD PUT - FLOAT");
      end;

      Set_Line_Length (Ft, 7);

      begin
         Put (Ft, "X");
         Put (Ft, Y, Fore => 1, Aft => 2, Exp => 1);          -- 1.34E+2
      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED - 3 FLOAT");
      end;

      begin
         Put (Ft, "Z");
         Put (Ft, Z, Fore => 1);
         Failed ("LAYOUT_ERROR NOT RAISED - FLOAT 2");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("SOME EXCEPTION RAISED - 3 FLOAT");
      end;

      Check_File (Ft, "1.34E+02#1.2E+02#X#1.34E+2#Z#@%");

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

end Ce3806e;
