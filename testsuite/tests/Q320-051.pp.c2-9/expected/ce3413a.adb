-- CE3413A.ADA

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
--     CHECK THAT PAGE RETURNS THE VALUE OF THE CURRENT PAGE NUMBER.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/29/82
--     JBG 08/30/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/04/87  REMOVED DEPENDENCE ON RESET AND CHECKED FOR
--                   USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3413a is
   Incomplete : exception;

begin

   Test ("CE3413A", "CHECK THAT PAGE RETURNS THE CORRECT PAGE " & "NUMBER");

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
            Failed ("UNEXPECTED EXCEPTION RAISED ON " & "TEXT CREATE");
            raise Incomplete;
      end;

      if Page (Ft) /= 1 then
         Failed ("CURRENT PAGE NOT INITIALLY ONE");
      end if;

      for I in 1 .. 6 loop
         Put (Ft, "OUTPUT STRING");
         New_Page (Ft);
      end loop;
      if Page (Ft) /= 7 then
         Failed ("PAGE INCORRECT AFTER PUT; IS" & Count'Image (Page (Ft)));
      end if;

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      if Page (Ft) /= 1 then
         Failed ("PAGE INCORRECT AFTER OPEN IS" & Count'Image (Page (Ft)));
      end if;

      for I in 1 .. 4 loop
         Skip_Page (Ft);
      end loop;
      if Page (Ft) /= 5 then
         Failed
           ("PAGE INCORRECT AFTER SKIP_PAGE; IS" & Count'Image (Page (Ft)));
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

end Ce3413a;
