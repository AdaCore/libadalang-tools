-- CE3705A.ADA

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
--     FOR GET FROM A FILE, CHECK THAT IF ONLY THE FILE TERMINATOR
--     REMAINS TO BE READ, THEN ANY CALL TO GET FOR AN INTEGER (EVEN
--     WITH WIDTH = 0) RAISES END_ERROR.

-- HISTORY:
--     BCB 10/28/88  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3705a is

   File : File_Type;

   Incomplete : exception;

   I : Integer;

   package Int_Io is new Integer_Io (Integer);
   use Int_Io;

begin
   Test
     ("CE3705A",
      "FOR GET FROM A FILE, CHECK THAT IF ONLY THE " &
      "FILE TERMINATOR REMAINS TO BE READ, THEN ANY " &
      "CALL TO GET FOR AN INTEGER (EVEN WITH WIDTH = " &
      "0) RAISES END_ERROR");

   begin
      begin
         Create (File, Out_File, Legal_File_Name);
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

      Put (File, 3);

      Close (File);

      Open (File, In_File, Legal_File_Name);

      Get (File, I);

      begin
         Get (File, I);
         Failed ("END_ERROR NOT RAISED - 1");
      exception
         when End_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 1");
      end;

      begin
         Get (File, I, Width => 0);
         Failed ("END_ERROR NOT RAISED - 2");
      exception
         when End_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 2");
      end;

      begin
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;
   exception
      when Incomplete =>
         null;
   end;

   Result;
end Ce3705a;
