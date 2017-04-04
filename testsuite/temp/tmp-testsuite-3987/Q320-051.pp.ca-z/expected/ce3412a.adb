-- CE3412A.ADA

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
--     CHECK THAT LINE RETURNS THE VALUE OF THE CURRENT LINE NUMBER.

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

procedure Ce3412a is
   Incomplete : exception;

begin

   Test ("CE3412A", "CHECK LINE RETURNS LINE NUMBER");

   declare
      Ft : File_Type;
      X  : Character;
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

      if Line (Ft) /= 1 then
         Failed ("CURRENT LINE NUMBER NOT INITIALLY ONE");
      end if;

      for I in 1 .. 3 loop
         Put (Ft, "OUTPUT STRING");
         New_Line (Ft);
      end loop;
      if Line (Ft) /= 4 then
         Failed ("LINE INCORRECT AFTER PUT; IS" & Count'Image (Line (Ft)));
      end if;

      New_Page (Ft);
      if Line (Ft) /= 1 then
         Failed
           ("LINE INCORRECT AFTER NEW_PAGE; IS" & Count'Image (Line (Ft)));
      end if;

      for I in 1 .. 5 loop
         Put (Ft, "MORE OUTPUT");
         New_Line (Ft);
      end loop;

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      if Line (Ft) /= 1 then
         Failed ("LINE INCORRECT AFTER RESET; IS" & Count'Image (Line (Ft)));
      end if;

      for I in 1 .. 2 loop
         Skip_Line (Ft);
      end loop;
      if Line (Ft) /= 3 then
         Failed
           ("LINE INCORRECT AFTER SKIP_LINE; IS" & Count'Image (Line (Ft)));
      end if;

      Set_Line (Ft, 2);
      if Line (Ft) /= 2 then
         Failed
           ("LINE INCORRECT AFTER SET_LINE; IS" & Count'Image (Line (Ft)));
      end if;

      Skip_Page (Ft);
      if Line (Ft) /= 1 then
         Failed
           ("LINE INCORRECT AFTER SKIP_PAGE; IS" & Count'Image (Line (Ft)));
      end if;

      begin
         Delete (Ft);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3412a;
