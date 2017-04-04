-- CE3112D.ADA

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
--     CHECK THAT AN EXTERNAL TEXT FILE SPECIFIED BY A NON-NULL STRING
--     NAME IS ACCESSIBLE AFTER THE COMPLETION OF THE MAIN PROGRAM.

--     THIS TEST CHECKS THE CREATION OF A TEXT FILE X3112C, WHICH WAS
--     CREATED BY CE3112C.ADA.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     GMT 08/13/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Text_Io;

procedure Ce3112d is

   Incomplete : exception;
   Check_Support, File_Name : Text_Io.File_Type;
   Prevent_Empty_File       : String (1 .. 5);

begin
   Test
     ("CE3112D",
      "CHECK THAT AN EXTERNAL TEXT FILE SPECIFIED BY " &
      "A NON-NULL STRING NAME IS ACCESSIBLE AFTER " &
      "THE COMPLETION OF THE MAIN PROGRAM");

   -- TEST FOR TEXT FILE SUPPORT.

   begin
      Text_Io.Create (Check_Support, Text_Io.Out_File, Legal_File_Name);
      begin
         Text_Io.Delete (Check_Support);
      exception
         when Text_Io.Use_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON " & "DELETE - 1");
      end;
   exception
      when Text_Io.Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT CREATE WITH " & "OUT_FILE MODE - 2");
         raise Incomplete;
      when Text_Io.Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE  - 3");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT " & "CREATE - 4");
         raise Incomplete;
   end;

   -- BEGIN TEST OBJECTIVE.

   begin
      Text_Io.Open
        (File_Name,
         Text_Io.In_File,
         Legal_File_Name (1, "CE3112C"));
   exception
      when Text_Io.Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON OPEN FOR TEXT " &
            "FILE WITH IN_FILE MODE - 5");
         raise Incomplete;
   end;

   Text_Io.Get (File_Name, Prevent_Empty_File);

   if Prevent_Empty_File /= "HELLO" then
      Failed ("OPENED WRONG FILE OR DATA ERROR - 6");
   end if;
   begin
      Text_Io.Delete (File_Name);
   exception
      when Text_Io.Use_Error =>
         Comment
           ("IMPLEMENTATION WOULD NOT ALLOW DELETION OF " &
            "EXTERNAL FILE - 7");
   end;

   Result;

exception
   when Incomplete =>
      Result;
end Ce3112d;
