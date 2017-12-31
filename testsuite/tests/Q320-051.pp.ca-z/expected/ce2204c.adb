-- CE2204C.ADA

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
--     CHECK THAT WRITE IS FORBIDDEN FOR SEQUENTIAL FILES OF
--     MODE IN_FILE.

--          B)  CHECK TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEMPORARY SEQUENTIAL FILES AND THE RESETTING FROM OUT_FILE
--     TO IN_FILE.

-- HISTORY:
--     GMT 07/27/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Sequential_Io;

procedure Ce2204c is
   Incomplete : exception;
begin
   Test
     ("CE2204C",
      "CHECK THAT MODE_ERROR IS RAISED BY WRITE " &
      "WHEN THE MODE IS INFILE AND THE FILE IS " & "A TEMPORARY FILE");
   declare
      package Seq_Io is new Sequential_Io (Integer);
      use Seq_Io;
      Ft   : File_Type;
      Var1 : Integer := 5;
   begin
      begin
         Create (Ft, Out_File);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON CREATE - 1");
            raise Incomplete;
      end;

      Write (Ft, Var1);

      begin
         Reset (Ft, In_File);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON RESET - 2");
            raise Incomplete;
      end;

      begin
         Write (Ft, 3);
         Failed ("MODE_ERROR NOT RAISED ON WRITE - 3");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON WRITE - 4");
      end;

      Close (Ft);
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2204c;
