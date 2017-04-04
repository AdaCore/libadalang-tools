-- CE3207A.ADA

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
--     CHECK THAT MODE_ERROR IS RAISED IF THE PARAMETER TO SET_INPUT HAS
--     MODE OUT_FILE OR THE PARAMETER TO SET_OUTPUT HAS MODE IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 07/07/88  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3207a is

   File1, File2 : File_Type;
   Incomplete : exception;

begin

   Test
     ("CE3207A",
      "CHECK THAT MODE_ERROR IS RAISED IF THE " &
      "PARAMETER TO SET_INPUT HAS MODE OUT_FILE " &
      "OR THE PARAMETER TO SET_OUTPUT HAS MODE " &
      "IN_FILE");

   begin

      begin
         Create (File1, Out_File);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE " & "WITH MODE OUT_FILE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      begin
         Set_Input (File1);
         Failed
           ("MODE_ERROR NOT RAISED FOR SET_INPUT WITH " & "MODE OUT_FILE");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR SET_INPUT");
      end;

      Create (File2, Out_File, Legal_File_Name);

      Put (File2, "OUTPUT STRING");
      Close (File2);
      Open (File2, In_File, Legal_File_Name);

      begin
         Set_Output (File2);
         Failed
           ("MODE_ERROR NOT RAISED FOR SET_OUTPUT WITH " & "MODE IN_FILE");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR SET_OUTPUT");
      end;

      begin
         Delete (File2);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3207a;
