-- CE3605C.ADA

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
--     CHECK THAT PUT RAISES MODE_ERROR FOR FILES OF MODE IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/02/82
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  REMOVED DEPENDENCE ON RESET, REMOVED UNNECESSARY
--                   CODE, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3605c is
   Incomplete : exception;

begin

   Test ("CE3605C", "MODE_ERROR RAISED BY PUT FOR IN_FILES");

   declare
      File1 : File_Type;
   begin

      begin
         Create (File1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON " & "TEXT CREATE");
            raise Incomplete;
      end;

      Put (File1, 'A');
      Close (File1);

      begin
         Open (File1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT " & "OPEN FOR IN_FILE MODE");
            raise Incomplete;
      end;

      begin
         Put (File1, 'A');
         Failed ("MODE_ERROR NOT RAISED - 1");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 1");
      end;

      begin
         Put (Standard_Input, 'A');
         Failed ("MODE_ERROR NOT RAISED - 2");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 2");
      end;

      begin
         Put (Current_Input, 'A');
         Failed ("MODE_ERROR NOT RAISED - 3");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 3");
      end;

      begin
         Put (File1, "STRING");
         Failed ("MODE_ERROR NOT RAISED - 4");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 4");
      end;

      begin
         Put (Standard_Input, "STRING");
         Failed ("MODE_ERROR NOT RAISED - 5");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 5");
      end;

      begin
         Put (Current_Input, "STRING");
         Failed ("MODE_ERROR NOT RAISED - 6");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 6");
      end;

      begin
         Delete (File1);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3605c;
