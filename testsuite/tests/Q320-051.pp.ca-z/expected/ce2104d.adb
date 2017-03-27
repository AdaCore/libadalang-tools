-- CE2104D.ADA

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

--          B) DIRECT FILES

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

with Direct_Io;
with Report; use Report;

procedure Ce2104d is

   package Dir_Io is new Direct_Io (Integer);
   use Dir_Io;
   type Acc_Str is access String;

   Dir_File_One  : Dir_Io.File_Type;
   Dir_File_Two  : Dir_Io.File_Type;
   Dir_File_Name : Acc_Str;
   Var           : Integer;
   Incomplete : exception;

begin

   Test
     ("CE2104D",
      "CHECK THAT THE NAME RETURNED BY NAME " &
      "CAN BE USED IN A SUBSEQUENT OPEN");

-- CREATE TEST FILE

   begin
      Create (Dir_File_One, Out_File, Legal_File_Name);
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

   Write (Dir_File_One, 3);
   Dir_File_Name := new String'(Name (Dir_File_One));
   Close (Dir_File_One);

-- ATTEMPT TO RE-OPEN DIRECT TEST FILE USING RETURNED NAME VALUE

   begin
      Open (Dir_File_Two, In_File, Dir_File_Name.all);
   exception
      when Dir_Io.Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
      when Dir_Io.Name_Error =>
         Failed ("STRING NOT ACCEPTED AS NAME FOR FILE - DIR");
         raise Incomplete;
      when others =>
         Failed ("FILE NOT RE-OPENED - DIR");
         raise Incomplete;

   end;

   Read (Dir_File_Two, Var);
   if Var /= 3 then
      Failed ("WRONG DATA RETURNED FROM READ - DIR");
   end if;

-- DELETE TEST FILE

   begin
      Delete (Dir_File_Two);
   exception
      when Use_Error =>
         Comment ("DELETION OF EXTERNAL FILE IS NOT SUPPORTED");
   end;

   Result;

exception

   when Incomplete =>
      Result;

end Ce2104d;