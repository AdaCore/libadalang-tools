-- CE3704N.ADA

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
--     CHECK THAT GET FOR INTEGER_IO RAISES DATA_ERROR WHEN:
--     (A) BASE LESS THAN 2 OR GREATER THAN 16
--     (B) THE LETTERS IN BASE ARE OUT OF THE BASE RANGE
--     (C) THERE IS NO CLOSING '#' SIGN FOR A BASED LITERAL

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     VKG 02/10/83
--     SPS 03/16/83
--     CPP 07/30/84
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  REMOVED UNNECESSARY CODE, CORRECTED
--                   EXCEPTION HANDLING, AND CHECKED FOR
--                   USE_ERROR ON DELETE.

with Text_Io; use Text_Io;
with Report;  use Report;

procedure Ce3704n is
   Incomplete : exception;

begin
   Test
     ("CE3704N",
      "CHECK THAT DATA_ERROR IS RAISED WHEN " &
      "A BASED LITERAL DOES NOT HAVE ITS BASE " &
      "IN THE RANGE 2 .. 16, DIGIT IS OUTSIDE " &
      "THE BASE RANGE, OR THERE IS NO CLOSING " & "'#' SIGN");

   declare
      Ft : File_Type;
   begin
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

      Put (Ft, "1#0000#");
      New_Line (Ft);
      Put (Ft, "A#234567#");
      New_Line (Ft);
      Put (Ft, "17#123#1");
      New_Line (Ft);
      Put (Ft, "5#1253#2");
      New_Line (Ft);
      Put (Ft, "8#123");
      Close (Ft);

      declare
         package Int_Io is new Integer_Io (Integer);
         use Int_Io;
         X  : Integer := 1_003;
         Ch : Character;
      begin
         begin
            Open (Ft, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable
                 ("USE_ERROR RAISED; TEXT " & "OPEN WITH IN_FILE MODE");
               raise Incomplete;
         end;

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - (1)");
         exception
            when Data_Error =>
               if X /= 1_003 then
                  Failed
                    ("ACTUAL PARAMETER TO GET " & "AFFECTED ON DATA_ERROR");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - (1)");
         end;

         if not End_Of_Line (Ft) then
            Get (Ft, Ch);
            Failed ("GET STOPPED AT WRONG POSITION - " & "(1): CHAR IS " & Ch);
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - (2)");
         exception
            when Data_Error =>
               if X /= 1_003 then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - (2)");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - (2)");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - (2)");
         else
            Get (Ft, Ch);
            if Ch /= 'A' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- (2): CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - (2A)");
         exception
            when Data_Error =>
               if X /= 1_003 then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - (2A)");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - (2A)");
         end;

         if not End_Of_Line (Ft) then
            Get (Ft, Ch);
            if Ch /= '1' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- (2A): CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - (3)");
         exception
            when Data_Error =>
               if X /= 1_003 then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - (3)");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - (3)");
         end;

         if not End_Of_Line (Ft) then
            Get (Ft, Ch);
            if Ch /= '2' then
               Failed
                 ("GET STOPPED AT WRONG POSITION - " & "(3): CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - (4)");
         exception
            when Data_Error =>
               if X /= 1_003 then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - (4)");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - (4)");
         end;

         if not End_Of_Line (Ft) then
            Get (Ft, Ch);
            if Ch /= ' ' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- (4): CHAR IS " & Ch);
            end if;
         end if;

      end;

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

end Ce3704n;
