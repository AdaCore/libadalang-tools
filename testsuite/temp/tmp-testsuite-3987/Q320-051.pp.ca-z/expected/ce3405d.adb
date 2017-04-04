-- CE3405D.ADA

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
--     CHECK THAT NEW_PAGE INCREMENTS THE CURRENT PAGE NUMBER AND
--     SETS THE CURRENT COLUMN AND LINE NUMBERS TO ONE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 08/28/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/23/87  CORRECTED EXCEPTION HANDLING AND ADDED CASES FOR
--                   CONSECUTIVE NEW_LINE AND NEW_PAGE.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3405d is
   Incomplete : exception;
begin

   Test
     ("CE3405D",
      "CHECK THAT NEW_PAGE INCREMENTS PAGE COUNT " &
      "AND SETS COLUMN AND LINE TO ONE");

   declare
      Ft     : File_Type;
      Ch     : Character;
      Pg_Num : Positive_Count;
   begin

      begin
         Create (Ft, Out_File);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " &
               "FOR TEMP FILE WITH OUT_FILE " &
               "MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
            raise Incomplete;
      end;

      Put (Ft, "STRING");
      New_Line (Ft);
      Put (Ft, 'X');
      Pg_Num := Page (Ft);

      New_Page (Ft);

      if Col (Ft) /= 1 then
         Failed ("COLUMN NUMBER NOT RESET - OUTPUT - 1");
      end if;
      if Line (Ft) /= 1 then
         Failed ("LINE NUMBER NOT RESET - OUTPUT - 1");
      end if;
      if Page (Ft) /= Pg_Num + 1 then
         Failed ("PAGE NUMBER NOT INCREMENTED - OUTPUT - 1");
      end if;

      Put (Ft, "MORE STUFF");
      New_Line (Ft);
      New_Page (Ft);

      if Col (Ft) /= 1 then
         Failed ("COLUMN NUMBER NOT RESET - OUTPUT - 2");
      end if;
      if Line (Ft) /= 1 then
         Failed ("LINE NUMBER NOT RESET - OUTPUT - 2");
      end if;
      if Page (Ft) /= Pg_Num + 2 then
         Failed ("PAGE NUMBER NOT INCREMENTED - OUTPUT - 2");
      end if;

      Check_File (Ft, "STRING#X#@MORE STUFF#@%");

      Close (Ft);

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3405d;
