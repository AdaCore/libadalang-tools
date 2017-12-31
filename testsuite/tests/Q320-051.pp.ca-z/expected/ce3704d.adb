-- CE3704D.ADA

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
--     CHECK THAT INTEGER_IO GET READS AT MOST WIDTH CHARACTERS
--     OR UP TO THE NEXT TERMINATOR; INCLUDING LEADING BLANKS
--     AND HORIZONTAL TABULATION CHARACTERS, WHEN WIDTH IS
--     NONZERO.

--     CHECK THAT INPUT TERMINATES WHEN A LINE TERMINATOR IS
--     ENCOUNTERED AND THAT DATA_ERROR IS RAISED IF THE DATA
--     READ IS INVALID.

-- APPLICABILITY CRITERIA:

--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/04/82
--     VKG 01/12/83
--     SPS 02/08/83
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  ADDED CASES FOR TABS, REMOVED UNNECESSARY
--                   CODE, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3704d is
   Incomplete : exception;

begin

   Test
     ("CE3704D",
      "CHECK THAT INTEGER_IO GET READS AT MOST " &
      "WIDTH CHARACTERS OR UP TO THE NEXT " &
      "TERMINATOR; INCLUDING LEADING BLANKS AND " &
      "HORIZONTAL TABULATION CHARACTERS, WHEN WIDTH " & "IS NONZERO");

   declare
      Ft : File_Type;
      X  : Integer;
      package Iio is new Integer_Io (Integer);
      use Iio;
   begin

-- CREATE AND INITIALIZE FILE

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      Put (Ft, "  123");
      New_Line (Ft);
      Put (Ft, "-5678");
      New_Line (Ft);
      Put (Ft, "  ");
      New_Page (Ft);
      Put (Ft, Ascii.Ht & "9");
      New_Page (Ft);

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

-- BEGIN TEST

      Get (Ft, X, 5);
      if X /= Ident_Int (123) then
         Failed ("WIDTH CHARACTERS NOT READ - 1");
      else
         begin
            Get (Ft, X, 2);
            Failed ("DATA_ERROR NOT RAISED - 1");
         exception
            when Data_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED -1");
         end;
         Skip_Line (Ft);
         Get (Ft, X, 6);
         if X /= Ident_Int (-5_678) then
            Failed ("GET WITH WIDTH " & "INCORRECT - 2");
         else
            begin
               Get (Ft, X, 2);
               Failed ("DATA_ERROR NOT RAISED - 2");
            exception
               when Data_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED - 2");
            end;
            Skip_Line (Ft);
            begin
               Get (Ft, X, 2);
               Failed ("DATA_ERROR NOT RAISED - 3");
            exception
               when Data_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED - 3");
            end;
            Skip_Line (Ft);
            Get (Ft, X, 2);
            if X /= Ident_Int (9) then
               Failed ("GET WITH WIDTH " & "INCORRECT - 3");
            end if;
         end if;
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

end Ce3704d;
