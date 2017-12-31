-- CE2202A.ADA

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
--     CHECK THAT READ, WRITE, AND END_OF_FILE RAISE STATUS_ERROR
--     WHEN APPLIED TO A NON-OPEN SEQUENTIAL FILE.  USE_ERROR IS
--     NOT PERMITTED.

-- HISTORY:
--     ABW 08/17/82
--     SPS 09/13/82
--     SPS 11/09/82
--     EG  11/26/84
--     EG  05/16/85
--     GMT 07/24/87  REPLACED CALL TO REPORT.COMMENT WITH "NULL;".

with Report; use Report;
with Sequential_Io;

procedure Ce2202a is

   package Seq is new Sequential_Io (Integer);
   use Seq;
   File1, File2 : File_Type;
   Cnst         : constant Integer := 101;
   Ival         : Integer;
   Bool         : Boolean;

begin
   Test
     ("CE2202A",
      "CHECK THAT READ, WRITE, AND " & "END_OF_FILE RAISE STATUS_ERROR " &
      "WHEN APPLIED TO A NON-OPEN " & "SEQUENTIAL FILE");
   begin
      begin
         Write (File1, Cnst);
         Failed
           ("STATUS_ERROR NOT RAISED WHEN WRITE APPLIED " &
            "TO NON-EXISTENT FILE");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED WHEN WRITE " &
               "APPLIED TO NON-EXISTENT FILE");
      end;

      begin
         Read (File1, Ival);
         Failed
           ("STATUS_ERROR NOT RAISED WHEN READ APPLIED " &
            "TO NON-EXISTENT FILE");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED WHEN READ " &
               "APPLIED TO NON-EXISTENT FILE");
      end;

      begin
         Bool := End_Of_File (File1);
         Failed
           ("STATUS_ERROR NOT RAISED WHEN END_OF_FILE " &
            "APPLIED TO NON-EXISTENT FILE");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED WHEN END_OF_FILE " &
               "APPLIED TO NON-EXISTENT FILE");
      end;
   end;

   begin
      begin
         Create (File2);
         Close (File2);
      exception
         when Use_Error =>
            null; -- IF FILE2 CANNOT BE CREATED THEN WE WILL
            -- BE REPEATING EARLIER TESTS, BUT THAT'S OK.
      end;

      begin
         Write (File2, Cnst);
         Failed ("STATUS_ERROR NOT RAISED WHEN WRITE APPLIED " & "TO FILE2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED WHEN WRITE " & "APPLIED TO FILE2");
      end;

      begin
         Read (File2, Ival);
         Failed ("STATUS_ERROR NOT RAISED WHEN READ APPLIED " & "TO FILE2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED WHEN READ " & "APPLIED TO FILE2");
      end;

      begin
         Bool := End_Of_File (File2);
         Failed
           ("STATUS_ERROR NOT RAISED WHEN END_OF_FILE " & "APPLIED TO FILE2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED WHEN END_OF_FILE " &
               "APPLIED TO FILE2");
      end;

   end;

   Result;

end Ce2202a;
