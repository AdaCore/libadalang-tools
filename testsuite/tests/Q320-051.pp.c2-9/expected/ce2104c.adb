-- CE2104C.ADA

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
--     CHECK THAT A FILE CAN BE CLOSED AND THEN RE-OPENED.

--          B) DIRECT FILES

-- APPLICABLILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHOSE
--     ENVIRONMENT SUPPORTS CREATE/OPEN FOR THE GIVEN MODE.

-- HISTORY:
--     DLD 08/11/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  06/03/85
--     PWB 02/10/86  CORRECTED REPORTED TEST NAME; CHANGED DATA FILE
--                   NAME TO "Y2104C" TO MATCH TEST NAME.
--     SPW 08/07/87  REMOVED UNNECESSARY CODE AND CORRECTED EXCEPTION
--                   HANDLING.

with Report; use Report;
with Direct_Io;

procedure Ce2104c is

   package Dir_Io is new Direct_Io (Integer);
   use Dir_Io;

   Dir_File : Dir_Io.File_Type;
   Var      : Integer;
   Incomplete : exception;

begin

   Test ("CE2104C", "CHECK THAT A FILE CAN BE CLOSED " & "AND THEN RE-OPENED");

-- INITIALIZE TEST FILE

   begin
      Create (Dir_File, Out_File, Legal_File_Name);
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

   Write (Dir_File, 28);
   Close (Dir_File);

-- RE-OPEN DIRECT TEST FILE

   begin
      Open (Dir_File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
   end;

   Read (Dir_File, Var);
   if Var /= 28 then
      Failed ("WRONG DATA RETURNED FROM READ - DIRECT");
   end if;

-- DELETE TEST FILE

   begin
      Delete (Dir_File);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception

   when Incomplete =>
      Result;

end Ce2104c;
