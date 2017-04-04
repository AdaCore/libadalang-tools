-- CE3602A.ADA

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
--     CHECK THAT GET FOR CHARACTERS AND STRINGS ALLOW A STRING TO SPAN
--     OVER MORE THAN ONE LINE, SKIPPING INTERVENING LINE AND PAGE
--     TERMINATORS.  ALSO CHECK THAT GET ACCEPTS A NULL STRING ACTUAL
--     PARAMETER AND A STRING SLICE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 08/30/82
--     VKG 01/26/83
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/04/87  REMOVED DEPENDENCE ON RESET, CORRECTED EXCEPTION
--                   HANDLING, AND ADDED NEW CASES FOR OBJECTIVE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3602a is
   Incomplete : exception;

begin

   Test
     ("CE3602A",
      "CHECK THAT GET FOR CHARACTERS AND STRINGS " &
      "ALLOWS A STRING TO SPAN OVER MORE THAN ONE " &
      "LINE, SKIPPING INTERVENING LINE AND PAGE " &
      "TERMINATORS.  ALSO CHECK THAT GET ACCEPTS " &
      "A NULL STRING ACTUAL PARAMETER AND A STRING " &
      "SLICE");

   declare
      File1                : File_Type;
      St                   : String (1 .. 40);
      Str                  : String (1 .. 100);
      Nst                  : String (1 .. 0);
      Original_Line_Length : Count;

-- READ_CHARS RETURNS A STRING OF N CHARACTERS FROM A GIVEN FILE.

      function Read_Chars (File : File_Type; N : Natural) return String is
         C : Character;
      begin
         if N = 0 then
            return "";
         else
            Get (File, C);
            return C & Read_Chars (File, N - 1);
         end if;
      exception
         when others =>
            Failed ("ERROR ON READ_CHARS");
      end Read_Chars;

   begin

-- CREATE AND INITIALIZE TEST DATA FILE

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
            Failed ("UNEXPECTED EXCEPTION RAISED ON " & "TEXT CREATE");
            raise Incomplete;
      end;

      Original_Line_Length := Line_Length;

-- LINE_LENGTH SET IN CASE IMPLEMENTATION REQUIRES BOUNDED LENGTH LINES

      Set_Line_Length (16);
      Put (File1, "THIS LINE SHALL ");
      Set_Line_Length (10);
      Put (File1, "SPAN OVER ");
      Set_Line_Length (14);
      Put (File1, "SEVERAL LINES.");
      Close (File1);
      Set_Line_Length (Original_Line_Length);

-- BEGIN TEST

      begin

         begin
            Open (File1, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable
                 ("USE_ERROR RAISED ON TEXT " & "OPEN WITH IN_FILE MODE - 1");
               raise Incomplete;
         end;

         Str (1 .. 40) := Read_Chars (File1, 40);
         Close (File1);

         Open (File1, In_File, Legal_File_Name);

         Get (File1, St);
         if Str (1 .. 40) /= St then
            Failed ("GET FOR STRING INCORRECT");
         end if;

         if Str (1 .. 40) /=
           "THIS LINE SHALL SPAN OVER SEVERAL " & "LINES."
         then
            Failed ("INCORRECT VALUE READ");
         end if;

-- GET NULL STRING

         Close (File1);

         Open (File1, In_File, Legal_File_Name);

         begin
            Get (File1, Nst);
         exception
            when others =>
               Failed (" GET FAILED ON NULL STRING");
         end;

-- GET NULL SLICE

         begin
            Get (File1, Str (10 .. 1));
         exception
            when others =>
               Failed ("GET FAILED ON A NULL SLICE");
         end;

         begin
            Delete (File1);
         exception
            when Use_Error =>
               null;
         end;

      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3602a;
