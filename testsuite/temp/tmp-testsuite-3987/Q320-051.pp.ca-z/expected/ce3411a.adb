-- CE3411A.ADA

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
--     CHECK THAT COL RETURNS THE VALUE OF THE CURRENT COLUMN NUMBER.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/29/82
--     JBG 08/30/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/02/87  REMOVED DEPENDENCE ON RESET AND CHECKED FOR
--                   USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3411a is
   Incomplete : exception;

begin

   Test
     ("CE3411A",
      "CHECK THAT COL RETURNS THE VALUE OF THE " & "CURRENT COLUMN NUMBER");

   declare
      Ft        : File_Type;
      X         : Character;
      Num_Chars : Positive_Count;
   begin

      begin
         Create (Ft, Out_File, Legal_File_Name);
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

      Put (Ft, "OUTPUT STRING");
      if Col (Ft) /= 14 then
         Failed ("COL INCORRECT AFTER PUT; IS" & Count'Image (Col (Ft)));
      end if;

      New_Line (Ft);
      if Col (Ft) /= 1 then
         Failed ("COL INCORRECT AFTER NEW_LINE; IS" & Count'Image (Col (Ft)));
      end if;

      Put (Ft, "MORE OUTPUT");
      New_Page (Ft);
      if Col (Ft) /= 1 then
         Failed ("COL INCORRECT AFTER NEW_PAGE; IS" & Count'Image (Col (Ft)));
      end if;

      Put (Ft, "FINAL");

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      if Col (Ft) /= 1 then
         Failed ("COL INCORRECT AFTER REOPEN; IS" & Count'Image (Col (Ft)));
      end if;

      for I in 1 .. 4 loop
         Get (Ft, X);
      end loop;
      if Col (Ft) /= 5 then
         Failed ("COL INCORRECT AFTER GET; IS" & Count'Image (Col (Ft)));
      end if;

      Num_Chars := Col (Ft);
      while not End_Of_Line (Ft) loop
         Get (Ft, X);
         Num_Chars := Num_Chars + 1;
      end loop;

      if Col (Ft) /= Num_Chars then
         Failed
           ("COL INCORRECT BEFORE END OF LINE; IS" & Count'Image (Col (Ft)));
      end if;

      Skip_Line (Ft);
      if Col (Ft) /= 1 then
         Failed ("COL INCORRECT AFTER SKIP_LINE; IS" & Count'Image (Col (Ft)));
      end if;

      Set_Col (Ft, 2);
      if Col (Ft) /= 2 then
         Failed ("COL INCORRECT AFTER SET_COL; IS" & Count'Image (Col (Ft)));
      end if;

      Skip_Page (Ft);
      if Col (Ft) /= 1 then
         Failed ("COL INCORRECT AFTER SKIP_PAGE; IS" & Count'Image (Col (Ft)));
      end if;

      begin
         Delete (Ft);
      exception
         when Use_Error =>
            null;
      end;

   end;

   Result;

exception
   when Incomplete =>
      Result;
end Ce3411a;
