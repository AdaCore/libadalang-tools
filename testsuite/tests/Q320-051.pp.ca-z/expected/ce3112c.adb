-- CE3112C.ADA

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
--     CHECK THAT AN EXTERNAL TEXT FILE SPECIFIED BY A NON-NULL
--     STRING NAME IS ACCESSIBLE AFTER THE COMPLETION OF THE MAIN
--     PROGRAM.

--     THIS TEST CREATES A TEXT FILE WHICH CE3112D.ADA WILL READ.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF AN EXTERNAL TEXT FILE WITH OUT_FILE MODE.

-- HISTORY:
--     GMT 08/13/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Text_Io;

procedure Ce3112c is

   Incomplete : exception;
   File_Name          : Text_Io.File_Type;
   Prevent_Empty_File : String (1 .. 5) := "HELLO";

begin
   Test
     ("CE3112C",
      "CHECK THAT AN EXTERNAL TEXT FILE SPECIFIED " &
      "BY A NON-NULL STRING NAME IS ACCESSIBLE " &
      "AFTER THE COMPLETION OF THE MAIN PROGRAM");
   begin
      begin
         Text_Io.Create (File_Name, Text_Io.Out_File, Legal_File_Name);
      exception
         when Text_Io.Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE - 1");
            raise Incomplete;
         when Text_Io.Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE - 2");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON " & "TEXT CREATE - 3");
            raise Incomplete;
      end;

      Text_Io.Put (File_Name, Prevent_Empty_File);
      Text_Io.Close (File_Name);

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3112c;
