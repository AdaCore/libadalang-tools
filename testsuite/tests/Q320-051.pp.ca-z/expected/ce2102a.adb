-- CE2102A.ADA

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
--     CHECK TO SEE THAT STATUS_ERROR IS RAISED WHEN PERFORMING ILLEGAL
--     OPERATIONS ON OPENED OR UNOPENED FILES OF TYPE SEQUENTIAL_IO.

--          A) OPENED FILES

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH OUT_FILE MODE FOR SEQUENTIAL FILES.

-- HISTORY:
--     DLD 08/10/82
--     JBG 02/22/84
--     SPW 07/29/87  SPLIT CASE FOR UNOPENED FILES INTO CE2102L.ADA.

with Report; use Report;
with Sequential_Io;

procedure Ce2102a is

   package Seq_Io is new Sequential_Io (Integer);
   use Seq_Io;
   Test_File_One : Seq_Io.File_Type;

begin

   Test
     ("CE2102A",
      "CHECK THAT STATUS_ERROR IS RAISED WHEN " &
      "PERFORMING ILLEGAL OPERATIONS ON OPENED FILES " &
      "OF TYPE SEQUENTIAL_IO");

   begin
      Create (Test_File_One, Out_File, Legal_File_Name);

-- CHECK THAT OPEN STATEMENT RAISES EXCEPTION WHEN FILE IS ALREADY OPEN

      begin
         Open (Test_File_One, Out_File, Legal_File_Name);
         Failed ("STATUS_ERROR NOT RAISED WHEN FILE IS " & "ALREADY OPEN - 1");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON OPEN - 1");
      end;

      begin
         Open (Test_File_One, In_File, Legal_File_Name);
         Failed ("STATUS_ERROR NOT RAISED WHEN FILE IS " & "ALREADY OPEN - 2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON OPEN - 2");
      end;

-- CHECK THAT CREATE STATEMENT RAISES EXCEPTION WHEN FILE IS ALREADY OPEN

      begin
         Create (Test_File_One, In_File, Legal_File_Name);
         Failed
           ("STATUS_ERROR NOT RAISED WHEN AN OPEN " &
            "FILE IS USED IN A CREATE - 1");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON CREATE - 1");
      end;

      begin
         Create (Test_File_One, Out_File, Legal_File_Name);
         Failed
           ("STATUS_ERROR NOT RAISED WHEN AN OPEN " &
            "FILE IS USED IN A CREATE - 2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON CREATE - 2");
      end;

--DELETE TEST FILE

      begin
         Delete (Test_File_One);
      exception
         when Use_Error =>
            Comment
              ("DELETION OF EXTERNAL FILE APPEARS NOT " & "TO BE SUPPORTED");

         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED " & "FOR DELETE");
      end;

   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED FOR CREATE " & "WITH OUT_FILE MODE");
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED FOR CREATE " & "WITH OUT_FILE MODE");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR CREATE");
   end;

   Result;
end Ce2102a;
