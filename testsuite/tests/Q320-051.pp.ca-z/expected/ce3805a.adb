-- CE3805A.ADA

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
--     CHECK THAT FLOAT_IO GET MAY READ THE LAST CHARACTER IN THE FILE
--     WITHOUT RAISNG END_ERROR AND THAT SUBSEQUENT READING WILL RAISE
--     END_ERROR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATAIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/08/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/15/87  REMOVED UNNECESSARY CODE AND CORRECTED EXCEPTION
--                   HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3805a is

begin

   Test
     ("CE3805A",
      "CHECK THAT FLOAT_IO GET MAY READ THE LAST " &
      "CHARACTER IN THE FILE WITHOUT RAISING " &
      "END_ERROR AND THAT SUBSEQUENT READING WILL " & "RAISE END_ERROR");

   declare
      Ft1, Ft2 : File_Type;
      package Fl_Io is new Float_Io (Float);
      X : Float;
      use Fl_Io;
      Incomplete : exception;

   begin

-- CREATE AND INITIALIZE TEST FILES

      begin
         Create (Ft1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      Create (Ft2, Out_File, Legal_File_Name (2));

      Put (Ft1, "2.25");
      Close (Ft1);

      Put (Ft2, "2.50");
      New_Line (Ft2, 3);
      New_Page (Ft2);
      New_Line (Ft2, 3);
      Close (Ft2);

-- BEGIN TEST

      begin
         Open (Ft1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT " & "OPEN WITH IN_FILE MODE");
            raise Incomplete;
      end;

      Open (Ft2, In_File, Legal_File_Name (2));

      begin
         Get (Ft1, X);
         if X /= 2.25 then
            Failed ("INCORRECT VALUE READ");
         end if;
         begin
            Get (Ft1, X);
            Failed ("END_ERROR NOT RAISED - 1");
         exception
            when End_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 1");
         end;
      exception
         when End_Error =>
            Failed ("END_ERROR RAISED PREMATURELY - 1");
         when others =>
            Failed ("UNEXPECTED ERROR RAISED - 1");
      end;

      begin
         Get (Ft2, X);
         if X /= 2.50 then
            Failed ("INCORRECT VALUE READ");
         end if;
         begin
            Get (Ft2, X);
            Failed ("END_ERROR NOT RAISED - 2");
         exception
            when End_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 2");
         end;
      exception
         when End_Error =>
            Failed ("END_ERROR RAISED PREMATURELY - 2");
         when others =>
            Failed ("UNEXPECTED ERROR RAISED - 2");
      end;

      begin
         Delete (Ft1);
         Delete (Ft2);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3805a;
