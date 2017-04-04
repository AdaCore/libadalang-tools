-- CE3604A.ADA

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
--     CHECK THAT GET_LINE MAY BE CALLED TO RETURN AN ENTIRE LINE.  ALSO
--     CHECK THAT GET_LINE MAY BE CALLED TO RETURN THE REMAINDER OF A
--     PARTLY READ LINE.  ALSO CHECK THAT GET_LINE RETURNS IN THE
--     PARAMETER LAST, THE INDEX VALUE OF THE LAST CHARACTER READ.
--     WHEN NO CHARACTERS ARE READ, LAST IS ONE LESS THAN ITEM'S LOWER
--     BOUND.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 09/25/87  COMPLETELY REVISED TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3604a is

begin

   Test
     ("CE3604A",
      "CHECK THAT GET_LINE READS LINES APPROPRIATELY " &
      "AND CHECK THAT LAST RETURNS THE CORRECT INDEX " &
      "VALUE");

   declare
      File  : File_Type;
      Str   : String (1 .. 25);
      Last  : Natural;
      Item1 : String (2 .. 6);
      Item2 : String (3 .. 6);
      Ch    : Character;
      Incomplete : exception;

   begin
      begin
         Create (File, Out_File, Legal_File_Name);
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
            Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT " & "CREATE");
            raise Incomplete;
      end;

      Put (File, "FIRST LINE OF INPUT");
      New_Line (File);
      Put (File, "SECOND LINE OF INPUT");
      New_Line (File);
      Put (File, "THIRD LINE OF INPUT");
      New_Line (File);
      New_Line (File);

      Close (File);

      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      Get_Line (File, Str, Last);

      begin
         if Str (1 .. Last) /= "FIRST LINE OF INPUT" then
            Failed ("GET_LINE - RETURN OF ENTIRE LINE");
         end if;
      exception
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED AFTER " & "GET_LINE - 1");
      end;

      Get (File, Item1);
      Get_Line (File, Str, Last);

      begin
         if Str (1 .. Last) /= "D LINE OF INPUT" then
            Failed ("GET_LINE - REMAINDER OF PARTLY READ LINE");
         end if;
      exception
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED AFTER " & "GET_LINE - 2");
      end;

      Get_Line (File, Item1, Last);
      if Last /= 6 then
         Failed ("INCORRECT VALUE FOR LAST PARAMETER - 1");
      end if;

      while not End_Of_Line (File) loop
         Get (File, Ch);
      end loop;

      Get_Line (File, Item1, Last);
      if Last /= 1 then
         Failed ("INCORRECT VALUE FOR LAST PARAMETER - 2");
      end if;

      if not End_Of_Line (File) then
         Failed ("END_OF_LINE NOT TRUE");
      end if;

      Get_Line (File, Item2, Last);
      if Last /= 2 then
         Failed ("INCORRECT VALUE FOR LAST PARAMETER - 3");
      end if;

      begin
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3604a;
