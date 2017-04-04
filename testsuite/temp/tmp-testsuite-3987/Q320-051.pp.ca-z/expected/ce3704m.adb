-- CE3704M.ADA

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
--     CHECK THAT GET FOR INTEGER_IO RAISES DATA_ERROR WHEN
--     THE INPUT CONTAINS
--
--     (1)  INTEGER_IO DECIMAL POINT
--     (2)  INTEGER_IO LEADING OR TRAILING UNDERSCORES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     VKG 02/10/83
--     CPP 07/30/84
--     EG  05/22/85
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  REMOVED UNNECESSARY CODE, CORRECTED
--                   EXCEPTION HANDLING, AND ADDED CASES WHICH
--                   CHECK GET AT THE END_OF_FILE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3704m is
   Incomplete : exception;

begin

   Test
     ("CE3704M",
      "CHECK THAT DATA_ERROR IS RAISED FOR " &
      "INTEGER_IO WHEN A DECIMAL POINT, OR " &
      "LEADING OR TRAILING UNDERSCORES " &
      "ARE DETECTED");

   declare
      Ft : File_Type;
      Ch : Character;
   begin

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      Put (Ft, "3.14152");
      New_Line (Ft);
      Put (Ft, "2.15");
      New_Line (Ft);
      Put (Ft, "_312");
      New_Line (Ft);
      Put (Ft, "-312_");

      Close (Ft);

      declare
         package Int_Io is new Integer_Io (Integer);
         use Int_Io;
         X : Integer := 402;
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
            Get (Ft, X, 3);
            Failed ("DATA_ERROR NOT RAISED - (1)");
         exception
            when Data_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - (1)");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - (1)");
         else
            Get (Ft, Ch);
            if Ch /= '4' then
               Failed
                 ("GET STOPPED AT WRONG " & "POSITION - (1): CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            if X /= 2 then
               Failed ("WRONG VALUE READ - (2)");
            end if;
         exception
            when Data_Error =>
               Failed ("DATA_ERROR RAISED - (2)");
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - (2)");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - (2)");
         else
            Get (Ft, Ch);
            if Ch /= '.' then
               Failed
                 ("GET STOPPED AT WRONG " & "POSITION - (2): CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - (3)");
         exception
            when Data_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - (3)");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - (3)");
         else
            Get (Ft, Ch);
            if Ch /= '_' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- (3): CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - (4)");
         exception
            when Data_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - (4)");
         end;

         if not End_Of_Line (Ft) then
            Failed ("END_OF_LINE NOT TRUE AFTER (4)");
         end if;

         begin
            Delete (Ft);
         exception
            when Use_Error =>
               null;
         end;
      end;
   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3704m;
