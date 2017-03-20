-- CE2104B.ADA

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
--     CHECK THAT THE NAME RETURNED BY NAME CAN BE USED IN A
--     SUBSEQUENT OPEN.

--          A) SEQUENTIAL FILES

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHOSE
--     ENVIRONMENT SUPPORTS CREATE/OPEN FOR THE GIVEN MODE.

-- HISTORY:
--     DLD 08/11/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/31/85
--     TBN 11/04/86  ADDED A RAISE INCOMPLETE STATEMENT WHEN FAILED IS
--                   CALLED FOR OPEN OR CREATE.
--     SPW 08/07/87  REMOVED UNNECESSARY CODE AND CORRECTED EXCEPTION
--                   HANDLING.

with Sequential_Io;
with Report; use Report;

procedure Ce2104b is

   package Seq_Io is new Sequential_Io (Integer);
   use Seq_Io;
   type Acc_Str is access String;

   Seq_File_One  : Seq_Io.File_Type;
   Seq_File_Two  : Seq_Io.File_Type;
   Seq_File_Name : Acc_Str;
   Var           : Integer;
   Incomplete : exception;

begin

   Test
     ("CE2104B",
      "CHECK THAT THE NAME RETURNED BY NAME " &
      "CAN BE USED IN A SUBSEQUENT OPEN");

-- CREATE TEST FILE

   begin
      Create (Seq_File_One, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE WITH " & "OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON CREATE WITH " & "OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
         raise Incomplete;
   end;

   Write (Seq_File_One, 14);
   Seq_File_Name := new String'(Name (Seq_File_One));
   Close (Seq_File_One);

-- ATTEMPT TO RE-OPEN SEQUENTIAL TEST FILE USING RETURNED NAME VALUE

   begin
      Open (Seq_File_Two, In_File, Seq_File_Name.all);
   exception
      when Seq_Io.Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
      when Seq_Io.Name_Error =>
         Failed ("STRING NOT ACCEPTED AS NAME FOR FILE - SEQ");
         raise Incomplete;
      when others =>
         Failed ("FILE NOT RE-OPENED - SEQ");
         raise Incomplete;
   end;

   Read (Seq_File_Two, Var);
   if Var /= 14 then
      Failed ("WRONG DATA RETURNED FROM READ -SEQ");
   end if;

-- DELETE TEST FILE

   begin
      Delete (Seq_File_Two);
   exception
      when Use_Error =>
         Comment ("DELETION OF EXTERNAL FILE IS NOT SUPPORTED");
   end;

   Result;

exception

   when Incomplete =>
      Result;

end Ce2104b;
