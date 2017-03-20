-- CE3601A.ADA

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
--     CHECK THAT GET (FOR STRINGS AND CHARACTERS), PUT (FOR STRINGS AND
--     CHARACTERS), GET_LINE, AND PUT_LINE RAISE STATUS_ERROR WHEN
--     CALLED WITH AN UNOPEN FILE PARAMETER.  ALSO CHECK NAMES OF FORMAL
--     PARAMETERS.

-- HISTORY:
--     SPS 08/27/82
--     VKG 02/15/83
--     JBG 03/30/83
--     JLH 09/04/87  ADDED CASE WHICH ATTEMPTS TO CREATE FILE AND THEN
--                   RETESTED OBJECTIVE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3601a is

begin

   Test
     ("CE3601A",
      "STATUS_ERROR RAISED BY GET, PUT, GET_LINE, " &
      "PUT_LINE WHEN FILE IS NOT OPEN");

   declare
      File1, File2 : File_Type;
      Ch           : Character := '%';
      Lst          : Natural;
      St           : String (1 .. 10);
      Ln           : String (1 .. 80);
   begin
      begin
         Get (File => File1, Item => Ch);
         Failed ("STATUS_ERROR NOT RAISED - GET CHARACTER");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET CHARACTER");
      end;

      begin
         Get (File => File1, Item => St);
         Failed ("STATUS_ERROR NOT RAISED - GET STRING");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET STRING");
      end;

      begin
         Get_Line (File => File1, Item => Ln, Last => Lst);
         Failed ("STATUS_ERROR NOT RAISED - GET_LINE");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET_LINE");
      end;

      begin
         Put (File => File1, Item => Ch);
         Failed ("STATUS_ERROR NOT RAISED - PUT CHARACTER");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT CHARACTER");
      end;

      begin
         Put (File => File1, Item => St);
         Failed ("STATUS_ERROR NOT RAISED - PUT STRING");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT STRING");
      end;

      begin
         Put_Line (File => File1, Item => Ln);
         Failed ("STATUS_ERROR NOT RAISED - PUT_LINE");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT_LINE");
      end;

      begin
         Create (File2, Out_File);   -- THIS IS ONLY AN ATTEMPT TO
         Close (File2);              -- CREATE A FILE. OK, WHETHER
      exception                        -- SUCCESSFUL OR NOT.
         when Use_Error =>
            null;
      end;

      begin
         Get (File => File2, Item => Ch);
         Failed ("STATUS_ERROR NOT RAISED - GET CHARACTER");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET CHARACTER");
      end;

      begin
         Get (File => File2, Item => St);
         Failed ("STATUS_ERROR NOT RAISED - GET STRING");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET STRING");
      end;

      begin
         Get_Line (File => File2, Item => Ln, Last => Lst);
         Failed ("STATUS_ERROR NOT RAISED - GET_LINE");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET_LINE");
      end;

      begin
         Put (File => File2, Item => Ch);
         Failed ("STATUS_ERROR NOT RAISED - PUT CHARACTER");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT CHARACTER");
      end;

      begin
         Put (File => File2, Item => St);
         Failed ("STATUS_ERROR NOT RAISED - PUT STRING");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT STRING");
      end;

      begin
         Put_Line (File => File2, Item => Ln);
         Failed ("STATUS_ERROR NOT RAISED - PUT_LINE");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT_LINE");
      end;

   end;

   Result;

end Ce3601a;
