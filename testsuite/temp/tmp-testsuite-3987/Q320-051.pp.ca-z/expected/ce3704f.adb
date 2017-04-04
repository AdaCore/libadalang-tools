-- CE3704F.ADA

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
--     CHECK THAT INTEGER_IO GET DOES NOT ALLOW EMBEDDED BLANKS OR
--     CONSECUTIVE UNDERSCORES TO BE INPUT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/04/82
--     VKG 01/14/83
--     CPP 07/30/84
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/10/87  REMOVED UNNECESSARY CODE, CORRECTED EXCEPTION
--                   HANDLING, AND ADDED MORE CHECKS OF THE VALUES
--                   OF CHARACTERS READ.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3704f is
   Incomplete : exception;

begin

   Test
     ("CE3704F",
      "INTEGER_IO GET DOES NOT ALLOW EMBEDDED " &
      "BLANKS OR CONSECUTIVE UNDERSCORES");

   declare
      Ft : File_Type;
      X  : Integer;
      package Iio is new Integer_Io (Integer);
      use Iio;
      Ch : Character;
      P  : Positive;
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

      Put (Ft, "12_345");
      New_Line (Ft);
      Put (Ft, "12 345");
      New_Line (Ft);
      Put (Ft, "1__345");
      New_Line (Ft);
      Put (Ft, "-56");
      New_Line (Ft);
      Put (Ft, "10E0");
      New_Line (Ft);
      Put (Ft, "10E-2X");
      New_Line (Ft);
      Put (Ft, "4E1__2");
      New_Line (Ft);
      Put (Ft, "1 0#99#");
      New_Line (Ft);
      Put (Ft, "1__0#99#");
      New_Line (Ft);
      Put (Ft, "10#9_9#");
      New_Line (Ft);
      Put (Ft, "10#9__9#");
      New_Line (Ft);
      Put (Ft, "10#9 9#");
      New_Line (Ft);
      Put (Ft, "16#E#E1");
      New_Line (Ft);
      Put (Ft, "2#110#E1_1");
      New_Line (Ft);
      Put (Ft, "2#110#E1__1");
      Close (Ft);

-- BEGIN TEST

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; " & "TEXT OPEN WITH IN_FILE " & "MODE");
            raise Incomplete;
      end;

      Get (Ft, X);
      if X /= 12_345 then
         Failed ("GET WITH UNDERSCORE INCORRECT - (1)");
      end if;

      Skip_Line (Ft);

      begin
         Get (Ft, X, 6);
         Failed ("DATA_ERROR NOT RAISED - (2)");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (2)");
      end;

      Skip_Line (Ft);

      begin
         Get (Ft, X);
         Failed ("DATA_ERROR NOT RAISED - (3)");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (3)");
      end;

      if End_Of_Line (Ft) then
         Failed ("GET STOPPED AT END OF LINE - (3)");
      else
         Get (Ft, Ch);
         if Ch /= '_' then
            Failed ("GET STOPPED AT WRONG POSITION - " & "(3): CHAR IS " & Ch);
         end if;
         Get (Ft, Ch);
         if Ch /= '3' then
            Failed
              ("GET STOPPED AT WRONG POSITION - " & "(3.5): CHAR IS " & Ch);
         end if;
      end if;

      Skip_Line (Ft);
      Get (Ft, X);
      if X /= (-56) then
         Failed ("GET WITH GOOD CASE INCORRECT - (4)");
      end if;

      Skip_Line (Ft);
      Get (Ft, X, 4);
      if X /= 10 then
         Failed ("GET WITH ZERO EXPONENT INCORRECT - (5)");
      end if;

      Skip_Line (Ft);

      begin
         Get (Ft, X);
         Failed ("DATA_ERROR NOT RAISED - (6)");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (6)");
      end;

      if End_Of_Line (Ft) then
         Failed ("GET STOPPED AT END OF LINE - (6)");
      else
         Get (Ft, Ch);
         if Ch /= 'X' then
            Failed ("GET STOPPED AT WRONG POSITION - " & "(6): CHAR IS " & Ch);
         end if;
      end if;

      Skip_Line (Ft);

      begin
         Get (Ft, X);
         Failed ("DATA_ERROR NOT RAISED - (7)");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (7)");
      end;

      if End_Of_Line (Ft) then
         Failed ("GET STOPPED AT END OF LINE - (7)");
      else
         Get (Ft, Ch);
         if Ch /= '_' then
            Failed ("GET STOPPED AT WRONG POSITION - " & "(7): CHAR IS " & Ch);
         end if;
         Get (Ft, Ch);
         if Ch /= '2' then
            Failed
              ("GET STOPPED AT WRONG POSITION - " & "(7.5): CHAR IS " & Ch);
         end if;
      end if;

      Skip_Line (Ft);

      begin
         Get (Ft, X, 7);
         Failed ("DATA_ERROR NOT RAISED - (8)");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (8)");
      end;

      Skip_Line (Ft);

      begin
         Get (Ft, X);
         Failed ("DATA_ERROR NOT RAISED - (9)");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (9)");
      end;

      if End_Of_Line (Ft) then
         Failed ("GET STOPPED AT END OF LINE - (9)");
      else
         Get (Ft, Ch);
         if Ch /= '_' then
            Failed ("GET STOPPED AT WRONG POSITION " & "- (9): CHAR IS " & Ch);
         end if;
         Get (Ft, Ch);
         if Ch /= '0' then
            Failed
              ("GET STOPPED AT WRONG POSITION " & "- (9.5): CHAR IS " & Ch);
         end if;
      end if;

      Skip_Line (Ft);
      Get (Ft, X);
      if X /= 99 then
         Failed ("GET WITH UNDERSCORE IN " & "BASED LITERAL INCORRECT - (10)");
      end if;

      Skip_Line (Ft);

      begin
         Get (Ft, X);
         Failed ("DATA_ERROR NOT RAISED - (11)");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (11)");
      end;

      if End_Of_Line (Ft) then
         Failed ("GET STOPPED AT END OF LINE - (11)");
      else
         Get (Ft, Ch);
         if Ch /= '_' then
            Failed
              ("GET STOPPED AT WRONG POSITION - " & "(11): CHAR IS " & Ch);
         end if;
         Get (Ft, Ch);
         if Ch /= '9' then
            Failed
              ("GET STOPPED AT WRONG POSITION - " & "(11.5): CHAR IS " & Ch);
         end if;
      end if;

      Skip_Line (Ft);

      begin
         Get (Ft, X, 6);
         Failed ("DATA_ERROR NOT RAISED - (12)");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (12)");
      end;

      Skip_Line (Ft);
      Get (Ft, X, 7);
      if X /= 224 then
         Failed ("GET WITH GOOD CASE OF " & "BASED LITERAL INCORRECT - (13)");
      end if;

      Skip_Line (Ft);
      Get (Ft, X, 10);
      if X /= (6 * 2**11) then
         Failed
           ("GET WITH UNDERSCORE IN EXPONENT" &
            "OF BASED LITERAL INCORRECT - (14)");
      end if;

      Skip_Line (Ft);

      begin
         Get (Ft, X);
         Failed ("DATA_ERROR NOT RAISED - (15)");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (15)");
      end;

      if End_Of_Line (Ft) then
         Failed ("GET STOPPED AT END OF LINE - (15)");
      else
         Get (Ft, Ch);
         if Ch /= '_' then
            Failed
              ("GET STOPPED AT WRONG POSITION - " & "(15): CHAR IS " & Ch);
         end if;
         Get (Ft, Ch);
         if Ch /= '1' then
            Failed
              ("GET STOPPED AT WRONG POSITION - " & "(15.5): CHAR IS " & Ch);
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

end Ce3704f;
