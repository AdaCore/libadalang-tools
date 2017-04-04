-- CE3414A.ADA

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
--     CHECK THAT STATUS_ERROR IS RAISED WHEN NEW_LINE, SKIP_LINE,
--     END_OF_LINE, NEW_PAGE, SKIP_PAGE, END_OF_PAGE, END_OF_FILE,
--     SET_COL, SET_LINE, COL, LINE, AND PAGE ARE CALLED AND THE FILE
--     IS NOT OPEN.

-- HISTORY:
--     BCB 10/27/88  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3414a is

   File : File_Type;

   Incomplete : exception;

   X : Positive_Count;

begin
   Test
     ("CE3414A",
      "CHECK THAT STATUS_ERROR IS RAISED WHEN " &
      "NEW_LINE, SKIP_LINE, END_OF_LINE, NEW_PAGE, " &
      "SKIP_PAGE, END_OF_PAGE, END_OF_FILE, SET_COL, " &
      "SET_LINE, COL, LINE, AND PAGE ARE CALLED AND " &
      "THE FILE IS NOT OPEN");

   begin
      begin
         Create (File, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      Put (File, 'A');

      Close (File);

      begin
         New_Line (File);
         Failed ("STATUS_ERROR WAS NOT RAISED - 1");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 1");
      end;

      begin
         Skip_Line (File);
         Failed ("STATUS_ERROR WAS NOT RAISED - 2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 2");
      end;

      begin
         if not End_Of_Line (File) then
            null;
         end if;
         Failed ("STATUS_ERROR WAS NOT RAISED - 3");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 3");
      end;

      begin
         New_Page (File);
         Failed ("STATUS_ERROR WAS NOT RAISED - 4");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 4");
      end;

      begin
         Skip_Page (File);
         Failed ("STATUS_ERROR WAS NOT RAISED - 5");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 5");
      end;

      begin
         if not End_Of_Page (File) then
            null;
         end if;
         Failed ("STATUS_ERROR WAS NOT RAISED - 6");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 6");
      end;

      begin
         if not End_Of_File (File) then
            null;
         end if;
         Failed ("STATUS_ERROR WAS NOT RAISED - 7");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 7");
      end;

      begin
         Set_Col (File, 2);
         Failed ("STATUS_ERROR WAS NOT RAISED - 8");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 8");
      end;

      begin
         Set_Line (File, 2);
         Failed ("STATUS_ERROR WAS NOT RAISED - 9");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 9");
      end;

      begin
         X := Col (File);
         Failed ("STATUS_ERROR WAS NOT RAISED - 10");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 10");
      end;

      begin
         X := Line (File);
         Failed ("STATUS_ERROR WAS NOT RAISED - 11");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 11");
      end;

      begin
         X := Page (File);
         Failed ("STATUS_ERROR WAS NOT RAISED - 12");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 12");
      end;
   exception
      when Incomplete =>
         null;
   end;

   Result;
end Ce3414a;
