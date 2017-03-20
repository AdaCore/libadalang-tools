-- CE2108F.ADA

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
--     CHECK THAT AN EXTERNAL SEQUENTIAL FILE SPECIFIED BY A NON-NULL
--     STRING NAME IS ACCESSIBLE AFTER THE COMPLETION OF THE MAIN
--     PROGRAM.

--     THIS TEST CHECKS THE CREATION OF A SEQUENTIAL FILE WHICH WAS
--     CREATED BY CE2108E.ADA.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     TBN 07/16/87  CREATED ORIGINAL TESTED.

with Report; use Report;
with Sequential_Io;

procedure Ce2108f is

   package Seq is new Sequential_Io (Integer);
   use Seq;
   Incomplete : exception;
   Check_Support, File_Name : File_Type;
   Prevent_Empty_File       : Natural := 0;

begin
   Test
     ("CE2108F",
      "CHECK THAT AN EXTERNAL SEQUENTIAL FILE " &
      "SPECIFIED BY A NON-NULL STRING NAME IS " &
      "ACCESSIBLE AFTER THE COMPLETION OF THE MAIN " &
      "PROGRAM");

   -- TEST FOR SEQUENTIAL FILE SUPPORT.

   begin
      Create (Check_Support, Out_File, Legal_File_Name);
      begin
         Delete (Check_Support);
      exception
         when Use_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON DELETE");
      end;
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON SEQUENTIAL " & "CREATE WITH OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON SEQUENTIAL " & "CREATE WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON " & "SEQUENTIAL CREATE");
         raise Incomplete;
   end;

   -- BEGIN TEST OBJECTIVE.

   begin
      Open (File_Name, In_File, Legal_File_Name (1, "CE2108E"));
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON OPEN FOR " &
            "SEQUENTIAL FILE WITH IN_FILE " &
            "MODE");
         raise Incomplete;
   end;
   Read (File_Name, Prevent_Empty_File);
   if Prevent_Empty_File /= 5 then
      Failed ("OPENED WRONG FILE OR DATA ERROR");
   end if;
   begin
      Delete (File_Name);
   exception
      when Use_Error =>
         Comment
           ("IMPLEMENTATION WOULD NOT ALLOW DELETION OF " & "EXTERNAL FILE");
   end;

   Result;

exception
   when Incomplete =>
      Result;
end Ce2108f;
