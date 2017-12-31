-- CE3411C.ADA

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
--     CHECK THAT COL OPERATES ON THE CURRENT DEFAULT OUTPUT FILE WHEN
--     NO FILE IS SPECIFIED.  CHECK THAT COL CAN OPERATE ON FILES OF
--     MODES IN_FILE AND OUT_FILE, INCLUDING THE CURRENT DEFAULT
--     INPUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/29/82
--     JBG 01/31/83
--     JBG 08/30/83
--     JLH 09/02/87  REMOVED DEPENDENCE ON RESET, REMOVED UNNECESSARY
--                   CODE, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3411c is
   Incomplete : exception;

begin

   Test
     ("CE3411C",
      "CHECK THAT COL OPERATES ON DEFAULT IN_FILE AND " & "OUT_FILE FILES");

   declare
      F1, F2 : File_Type;
      C      : Positive_Count;
      X      : Character;
   begin
      if Col /= Col (Standard_Output) then
         Failed ("COL DEFAULT NOT STANDARD_OUTPUT");
      end if;

      if Col /= Col (Standard_Input) then
         Failed ("COL DEFAULT NOT STANDARD_INPUT");
      end if;

      if Col /= Col (Current_Input) then
         Failed ("COL DEFAULT NOT CURRENT_INPUT");
      end if;

      if Col /= Col (Current_Output) then
         Failed ("COL DEFAULT NOT CURRENT_OUTPUT");
      end if;

      begin
         Create (F1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE WITH " & "OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      Create (F2, Out_File);

      Set_Output (F2);

      Put (F1, "STRING");
      if Col (F1) /= 7 then
         Failed ("COL INCORRECT SUBTEST 1");
      end if;

      Put (F2, "OUTPUT STRING");
      if Col /= Col (F2) and Col (F2) /= 14 then
         Failed
           ("COL INCORRECT SUBTEST 2; WAS " & Count'Image (Col) & " VS. " &
            Count'Image (Col (F2)));
      end if;

      Close (F1);

      begin
         Open (F1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      Set_Input (F1);

      Get (F1, X);
      Get (F1, X);
      Get (F1, X);

      if X /= 'R' then
         Failed ("INCORRECT VALUE READ");
      end if;

      if Col (Current_Input) /= 4 and Col /= 4 then
         Failed ("COL INCORRECT SUBTEST 3");
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

end Ce3411c;
