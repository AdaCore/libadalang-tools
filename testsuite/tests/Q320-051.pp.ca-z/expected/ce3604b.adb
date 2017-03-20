-- CE3604B.ADA

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
--     CHECK THAT GET_LINE DOES NOT DO A SKIP_LINE AND NO CHARACTERS ARE
--     READ WHEN THE INPUT IS AT THEN END OF A LINE AND THE STRING
--     PARAMETER IS A NULL STRING.  ALSO CHECK THAT GET_LINE DOES NOT
--     SKIP THE LINE TERMINATOR AFTER READING ALL THE CHARACTERS INTO
--     A STRING WHICH IS EXACTLY EQUAL TO THE NUMBER OF CHARACTERS
--     REMAINING ON THAT LINE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 10/13/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3604b is

begin

   Test ("CE3604B", "CHECK THAT GET_LINE READS LINES APPROPRIATELY");

   declare
      Incomplete : exception;
      File      : File_Type;
      Item1     : String (1 .. 19);
      Item2     : String (1 .. 20);
      Null_Item : String (2 .. 1);
      Last      : Natural;

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

      Close (File);

      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      Get (File, Item1);
      if Item1 /= "FIRST LINE OF INPUT" then
         Failed ("INCORRECT VALUE FOR GET");
      end if;

      Get_Line (File, Null_Item, Last);

      if Line (File) /= 1 then
         Failed ("INCORRECT LINE NUMBER AFTER GET_LINE - 1");
      end if;

      if Col (File) /= 20 then
         Failed ("INCORRECT COLUMN NUMBER AFTER GET_LINE - 1");
      end if;

      Skip_Line (File);
      Get_Line (File, Item2, Last);
      if Item2 /= "SECOND LINE OF INPUT" then
         Failed ("INCORRECT VALUE FOR GET_LINE");
      end if;

      if Line (File) /= 2 then
         Failed ("INCORRECT LINE NUMBER AFTER GET_LINE - 2");
      end if;

      if Col (File) /= 21 then
         Failed ("INCORRECT COLUMN NUMBER AFTER GET_LINE - 2");
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

end Ce3604b;
