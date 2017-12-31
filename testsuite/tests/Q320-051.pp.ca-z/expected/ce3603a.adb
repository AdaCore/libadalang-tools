-- CE3603A.ADA

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
--     CHECK THAT END_ERROR IS NOT RAISED BY:
--       GET FOR CHARACTERS UNTIL ONLY LINE AND PAGE TERMINATORS REMAIN;
--       GET FROM STRING UNTIL FEWER CHARACTERS THAN NEEDED REMAIN;
--       GET_LINE UNTIL THE FINAL PAGE TERMINATOR HAS BEEN SKIPPED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 08/31/82
--     JBG 12/23/82
--     EG  05/22/85
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  CORRECTED EXCEPTION HANDLING AND REMOVED
--                   DEPENDENCE ON RESET.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3603a is
   Incomplete : exception;

begin

   Test
     ("CE3603A",
      "CHECK THAT END_ERROR IS RAISED BY GET AFTER " &
      "THE LAST CHARACTER IN THE FILE HAS BEEN READ");

   declare
      File1     : File_Type;
      Oldch, Ch : Character;
      St        : String (1 .. 10) := (1 .. 10 => '.');
      Count     : Natural;
   begin

      begin
         Create (File1, Out_File, Legal_File_Name);
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
            Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT" & "CREATE");
            raise Incomplete;
      end;

      Put (File1, "LINE ONE");
      New_Line (File1);
      Put (File1, "LINE TWO");
      New_Line (File1, 3);
      New_Page (File1);
      New_Page (File1);
      Close (File1);

      begin

         begin
            Open (File1, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable
                 ("USE_ERROR RAISED ON TEXT " & "OPEN WITH IN_FILE MODE");
               raise Incomplete;
         end;

         Skip_Line (File1);
         Get (File1, St (1 .. 7));
         if St (1 .. 7) /= "LINE TW" then
            Failed ("NOT POSITIONED RIGHT - GET CHAR");
         end if;

-- COUNT NUMBER OF CHARACTERS IN FIRST LINE (TO ALLOW FOR TRAILING
--     BLANKS)

         Count := 0;
         while not End_Of_Line (File1) loop
            Get (File1, Ch);
            Oldch := Ch;
            Count := Count + 1;
         end loop;

         begin
            Get (File1, Ch);
            Failed ("END_ERROR NOT RAISED - GET " & "CHARACTER");
         exception
            when End_Error =>
               if Ch /= Oldch then
                  Failed ("CH MODIFIED ON END_" & "ERROR");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED " & "- GET CHARACTER");
         end;

         Close (File1);

         Open (File1, In_File, Legal_File_Name);

         Skip_Line (File1);
         Get (File1, St (1 .. 7));
         if St (1 .. 7) /= "LINE TW" then
            Failed ("WRONG LINE 2. ACTUALLY READ '" & St (1 .. 7) & "'");
         end if;

         begin
            Get (File1, St (8 .. 8 + Count));
            Failed ("END_ERROR NOT RAISED - GET " & "STRING");
         exception
            when End_Error =>
               if St (1 .. 7) /= "LINE TW" then
                  Failed ("ST MODIFIED ON END_ERROR");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED " & "- GET STRING");
         end;

         Close (File1);

      end;

      declare
         Last : Natural;
      begin

         Open (File1, In_File, Legal_File_Name);

         Skip_Line (File1);
         Get_Line (File1, St, Last);
         if Last < 8 then
            Failed ("LAST < 8.  LAST IS" & Integer'Image (Last));
         elsif St (1 .. 8) /= "LINE TWO" then
            Failed ("GET_LINE FAILED. ACTUALLY READ '" & St (1 .. 8) & "'");
         end if;

         Skip_Page (File1);
         Skip_Page (File1);

         begin
            Get_Line (File1, St (1 .. 1), Last);
            Failed ("END_ERROR NOT RAISED - GET_LINE - 1");
         exception
            when End_Error =>
               if Last /= 8 then
                  Failed
                    ("LAST MODIFIED BY GET_LINE " & "ON END_ERROR.  LAST IS" &
                     Integer'Image (Last));
               end if;
            when others =>
               Failed ("WRONG EXCEPTION - GET_LINE - 1");
         end;

         begin     -- NULL ITEM ARGUMENT
            Get_Line (File1, St (1 .. 0), Last);
         exception
            when End_Error =>
               Failed ("GET_LINE ATTEMPTED TO READ INTO A " & "NULL STRING");
            when others =>
               Failed ("WRONG EXCEPTION - GET_LINE - 2");
         end;
      end;

      begin
         Delete (File1);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3603a;
