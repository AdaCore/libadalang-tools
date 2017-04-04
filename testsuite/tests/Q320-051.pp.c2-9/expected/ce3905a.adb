-- CE3905A.ADA

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
--     CHECK THAT GET FOR ENUMERATION TYPES OPERATES ON FILE OF MODE
--     IN_FILE AND THAT WHEN NO FILE IS SPECIFIED IT OPERATES ON THE
--     CURRENT DEFAULT INPUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/07/82
--     SPS 12/22/82
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/16/87  REMOVED DEPENDENCE ON RESET AND CORRECTED
--                   EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3905a is
   Incomplete : exception;

begin

   Test
     ("CE3905A",
      "CHECK THAT GET FOR ENUMERATION TYPES " &
      "OPERATES ON FILE OF MODE IN_FILE AND THAT " &
      "WHEN NO FILE IS SPECIFIED IT OPERATES ON " &
      "THE CURRENT DEFAULT INPUT_FILE");

   declare
      type Day is (Monday, Tuesday, Wednesday, Thursday, Friday);
      package Day_Io is new Enumeration_Io (Day);
      Ft   : File_Type;
      File : File_Type;
      use Day_Io;
      X : Day;
   begin

-- CREATE AND INITIALIZE DATA FILES.

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE - 1");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE - 1");
            raise Incomplete;
      end;

      Put (Ft, "WEDNESDAY");
      New_Line (Ft);
      Put (Ft, "FRIDAY");

      Create (File, Out_File, Legal_File_Name (2));

      Put (File, "TUESDAY");
      New_Line (File);
      Put (File, "THURSDAY");

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT OPEN " & "FOR IN_FILE MODE");
            raise Incomplete;
      end;

      Close (File);
      Open (File, In_File, Legal_File_Name (2));

      Set_Input (File);

-- BEGIN TEST

      Get (Ft, X);
      if X /= Wednesday then
         Failed ("VALUE FROM FILE INCORRECT");
      end if;

      Get (X);
      if X /= Tuesday then
         Failed ("VALUE FROM DEFAULT INCORRECT");
      end if;

      Get (Ft, X);
      if X /= Friday then
         Failed ("VALUE FROM FILE INCORRECT");
      end if;

      Get (File, X);
      if X /= Thursday then
         Failed ("VALUE FROM DEFAULT INCORRECT");
      end if;

      begin
         Delete (Ft);
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

end Ce3905a;
