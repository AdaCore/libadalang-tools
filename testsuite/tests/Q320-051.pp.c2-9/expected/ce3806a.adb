-- CE3806A.ADA

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
--     CHECK THAT PUT FOR FLOAT_IO RAISES MODE_ERROR FOR FILES OF
--     MODE IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/10/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/11/87  REMOVED DEPENDENCE ON RESET AND CORRECTED
--                   EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3806a is

begin

   Test
     ("CE3806A",
      "CHECK THAT PUT FOR FLOAT_IO RAISES MODE_ERROR " &
      "FOR FILES OF MODE IN_FILE");

   declare
      Ft1 : File_Type;
      package Fl_Io is new Float_Io (Float);
      use Fl_Io;
      Incomplete : exception;
      X : Float := -34.267 / 19.2;

   begin

      begin
         Create (Ft1, Out_File, Legal_File_Name);
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

      Put (Ft1, 'A');
      Close (Ft1);

      begin
         Open (Ft1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      begin
         Put (Ft1, X);
         Failed ("MODE_ERROR NOT RAISED - 1");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 1");
      end;

      begin
         Put (Standard_Input, X);
         Failed ("MODE_ERROR NOT RAISED - 2");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 2");
      end;

      begin
         Put (Current_Input, X);
         Failed ("MODE_ERROR NOT RAISED - 3");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 3");
      end;

      begin
         Delete (Ft1);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3806a;
