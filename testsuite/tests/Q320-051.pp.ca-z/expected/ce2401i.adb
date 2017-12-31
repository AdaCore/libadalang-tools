-- CE2401I.ADA

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
--     CHECK THAT READ (WITH AND WITHOUT PARAMETER FROM), WRITE (WITH
--     AND WITHOUT PARAMETER TO), SET_INDEX, INDEX, SIZE, AND
--     END_OF_FILE ARE SUPPORTED FOR DIRECT FILES WITH ELEMENT_TYPE
--     FIXED POINT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY FOR IMPLEMENTATIONS WHICH SUPPORT CREATION OF
--     DIRECT FILES WITH INOUT_FILE MODE AND OPENING OF DIRECT FILES
--     WITH IN_FILE MODE.

-- HISTORY:
--     DWC 08/10/87  CREATED ORIGINAL VERSION.

with Report; use Report;
with Direct_Io;

procedure Ce2401i is

   End_Subtest : exception;

begin

   Test
     ("CE2401I",
      "CHECK THAT READ, WRITE, SET_INDEX, " &
      "INDEX, SIZE, AND END_OF_FILE ARE " &
      "SUPPORTED FOR DIRECT FILES WITH " & "ELEMENT_TYPE FIXED");

   declare

      type Fix_Type is delta 0.5 range 0.0 .. 255.0;
      package Dir_Fix is new Direct_Io (Fix_Type);
      use Dir_Fix;
      File_Fix : File_Type;

   begin
      begin
         Create (File_Fix, Inout_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - FIXED POINT");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON " & "CREATE - FIXED POINT");
            raise End_Subtest;
      end;

      declare
         Fix      : Fix_Type       := 16.0;
         Item_Fix : Fix_Type;
         One_Fix  : Positive_Count := 1;
         Two_Fix  : Positive_Count := 2;

      begin
         begin
            Write (File_Fix, Fix);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "FIXED POINT - 1");
         end;

         begin
            Write (File_Fix, Fix, Two_Fix);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "FIXED POINT - 2");
         end;

         begin
            if Size (File_Fix) /= Two_Fix then
               Failed ("SIZE FOR TYPE FIXED POINT");
            end if;

            if not End_Of_File (File_Fix) then
               Failed ("WRONG END_OF_FILE VALUE FOR " & "FIXED POINT");
            end if;

            Set_Index (File_Fix, One_Fix);

            if Index (File_Fix) /= One_Fix then
               Failed ("WRONG INDEX VALUE FOR FIXED " & "POINT");
            end if;
         end;

         Close (File_Fix);

         begin
            Open (File_Fix, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE MODE " & "NOT SUPPORTED");
               raise End_Subtest;
         end;

         begin
            Read (File_Fix, Item_Fix);
            if Item_Fix /= Fix then
               Failed ("WRONG VALUE READ FOR FIXED POINT");
            end if;
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR FIXED " & "POINT");
         end;

         begin
            Read (File_Fix, Item_Fix, One_Fix);
            if Item_Fix /= Fix then
               Failed ("WRONG VALUE READ WITH INDEX " & "FOR FIXED POINT");
            end if;
         exception
            when others =>
               Failed ("READ WITH FROM FOR FIXED POINT");
         end;

         begin
            Delete (File_Fix);
         exception
            when Use_Error =>
               null;
         end;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   Result;

end Ce2401i;
