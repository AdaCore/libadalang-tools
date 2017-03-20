-- CE2110A.ADA

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
--     CHECK THAT AN EXTERNAL FILE CEASES TO EXIST AFTER A SUCCESSFUL
--     DELETE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION AND DELETION OF SEQUENTIAL FILES.

-- HISTORY:
--     SPS 08/25/82
--     SPS 11/09/82
--     JBG 04/01/83
--     EG  05/31/85
--     JLH 07/21/87  ADDED A CALL TO NOT_APPLICABLE, IF EXCEPTION
--                   USE_ERROR IS RAISED BY DELETE.

with Report; use Report;
with Sequential_Io;

procedure Ce2110a is
begin

   Test
     ("CE2110A",
      "CHECK THAT THE EXTERNAL FILE CEASES TO EXIST " &
      "AFTER A SUCCESSFUL DELETE");

   declare
      package Seq is new Sequential_Io (Integer);
      use Seq;
      Fl1, Fl2 : File_Type;
      Var1     : Integer := 5;
      Incomplete : exception;
   begin
      begin
         Create (Fl1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      begin
         Write (Fl1, Var1);     -- THIS WRITES TO THE FILE IF IT
      exception                   -- CAN, NOT NECESSARY FOR THE
         when others =>         -- OBJECTIVE.
            null;
      end;

      begin
         Delete (Fl1);
      exception
         when Use_Error =>
            Not_Applicable ("DELETION OF EXTERNAL FILES NOT " & "SUPPORTED");
            raise Incomplete;
      end;

      begin
         Open (Fl2, In_File, Legal_File_Name);
         Failed
           ("EXTERNAL FILE STILL EXISTS AFTER " &
            "A SUCCESSFUL DELETION - SEQ");
      exception
         when Name_Error =>
            null;
      end;
   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce2110a;
