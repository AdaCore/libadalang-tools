-- CE3602C.ADA

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
--     CHECK THAT GET RAISES MODE_ERROR FOR FILES OF MODE OUT_FILE.

-- APPLICABILITY CRITEIRA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 08/31/82
--     SPS 12/17/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  CORRECTED EXCEPTION HANDLING AND CHECKED FOR
--                   USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3602c is
   Incomplete : exception;

begin

   Test
     ("CE3602C",
      "CHECK THAT MODE_ERROR IS RAISED BY GET FOR " &
      "FILES OF MODE OUT_FILE");

   declare
      File1, File2 : File_Type;
      Ch           : Character;
      St           : String (1 .. 5);
   begin

      begin
         Create (File1, Out_File);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " &
               "FOR TEMPORARY FILE WITH " & "OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON " & "TEXT CREATE - 1");
            raise Incomplete;
      end;

      begin
         Create (File2, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT " & "CREATE - 2");
            raise Incomplete;
      end;

      begin
         Get (File1, Ch);
         Failed ("MODE_ERROR NOT RAISED - GET CHAR UN-NAMED " & "FILE");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET CHAR " & "UN-NAMED FILE");
      end;

      begin
         Get (File2, Ch);
         Failed ("MODE_ERROR NOT RAISED - GET CHAR NAMED FILE");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET CHAR " & "NAMED FILE");
      end;

      begin
         Get (Standard_Output, Ch);
         Failed ("MODE_ERROR NOT RAISED - GET CHAR " & "STANDARD_OUTPUT");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET CHAR " & "STANDARD_OUTPUT");
      end;

      begin
         Get (Current_Output, Ch);
         Failed ("MODE_ERROR NOT RAISED - GET CHAR " & "CURRENT_OUTPUT");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET CHAR " & "CURRENT_OUTPUT");
      end;

      begin
         Get (File1, St);
         Failed ("MODE_ERROR NOT RAISED - GET STRING UN-NAMED " & "FILE");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET STRING " & "UN-NAMED FILE");
      end;

      begin
         Get (File2, St);
         Failed ("MODE_ERROR NOT RAISED - GET STRING NAMED FILE");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET STRING " & "NAMED FILE");
      end;

      begin
         Get (Standard_Output, St);
         Failed ("MODE_ERROR NOT RAISED - GET STRING " & "STANDARD_OUTPUT");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED - GET STRING " & "STANDARD_OUTPUT");
      end;

      begin
         Get (Current_Output, St);
         Failed ("MODE_ERROR NOT RAISED - GET STRING " & "CURRENT_OUTPUT");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET STRING " & "CURRENT_OUTPUT");
      end;

      Close (File1);

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

end Ce3602c;
