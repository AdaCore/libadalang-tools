-- CE3413C.ADA

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
--     CHECK THAT PAGE OPERATES ON THE CURRENT DEFAULT OUTPUT FILE WHEN
--     NO FILE IS SPECIFIED.  CHECK THAT PAGE CAN OPERATE ON FILES OF
--     MODES IN_FILE AND OUT_FILE, INCLUDING THE CURRENT DEFAULT
--     INPUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/29/82
--     JBG 08/30/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/04/87  REMOVED DEPENDENCE ON RESET, CORRECTED EXCEPTION
--                   HANDLING, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3413c is
   Incomplete : exception;

begin

   Test
     ("CE3413C",
      "CHECK THAT PAGE OPERATES ON DEFAULT IN_FILE " & "AND OUT_FILE FILES");

   declare
      F1, F2 : File_Type;
      C      : Positive_Count;
      X      : Character;
   begin

      begin
         Create (F1, Out_File, Legal_File_Name);
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

      begin
         Create (F2, Out_File);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " &
               "FOR TEMPORARY FILES WITH " &
               "OUT_FILE MODE");
            raise Incomplete;
      end;

      Set_Output (F2);

      if Page (F2) /= 1 and Page (Standard_Output) /= 1 then
         Failed ("PAGE INCORRECT SUBTEST - 1");
      end if;

      for I in 1 .. 3 loop
         Put (F1, "STRING");
         New_Page (F1);
      end loop;

      if Page (F1) /= 4 then
         Failed ("PAGE INCORRECT SUBTEST - 2");
      end if;

      Set_Line_Length (F2, 3);
      Set_Page_Length (F2, 1);
      Put ("OUTPUT STRING");
      if Page /= Page (F2) then
         Failed ("PAGE INCORRECT SUBTEST - 3");
      end if;

      Close (F1);

      begin
         Open (F1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON TEXT OPEN " & "IN_FILE MODE");
            raise Incomplete;
      end;

      Set_Input (F1);

      if Page (F1) /= 1 then
         Failed ("PAGE INCORRECT SUBTEST - 4");
      end if;

      Skip_Page (F1);
      Skip_Page (F1);
      if Page (F1) /= Page (Current_Input) then
         Failed ("PAGE INCORRECT SUBTEST - 5");
      end if;

      begin
         Delete (F1);
      exception
         when Use_Error =>
            null;
      end;

      Close (F2);

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3413c;
