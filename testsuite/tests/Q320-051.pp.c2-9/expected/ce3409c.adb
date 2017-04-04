-- CE3409C.ADA

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
--     CHECK THAT SET_COL SETS THE CURRENT COLUMN NUMBER TO THE VALUE
--     SPECIFIED BY TO FOR FILES OF MODES IN_FILE AND OUT_FILE.
--     CHECK THAT IT HAS NO EFFECT IF THE VALUE SPECIFIED BY TO IS
--     EQUAL TO THE CURRENT COLUMN NUMBER FOR BOTH IN_FILE AND OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/20/82
--     JBG 01/27/83
--     SPS 02/18/83
--     EG  05/22/85
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/31/87  REMOVED DEPENDENCE ON RESET, REMOVED UNNECESSARY
--                   CODE, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3409c is

   Incomplete : exception;

begin
   Test
     ("CE3409C",
      "CHECK THAT SET_COL SETS THE CURRENT COLUMN " &
      "NUMBER TO THE VALUE SPECIFIED BY TO FOR FILES " &
      "OF MODES IN_FILE AND OUT_FILE.  CHECK THAT IT " &
      "HAS NO EFFECT IF THE VALUE SPECIFIED BY TO IS " &
      "EQUAL TO THE CURRENT COLUMN NUMBER FOR BOTH " &
      "IN_FILE AND OUT_FILE");

   declare
      File      : File_Type;
      Char      : Character      := ('C');
      Item_Char : Character;
      One       : Positive_Count := Positive_Count (Ident_Int (1));
      Two       : Positive_Count := Positive_Count (Ident_Int (2));
      Four      : Positive_Count := Positive_Count (Ident_Int (4));
   begin

      begin
         Create (File, Out_File, Legal_File_Name);
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
            Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT " & "CREATE");
            raise Incomplete;
      end;

      Set_Page_Length (File, Two);
      Set_Col (File, Four);
      if Col (File) /= Four then
         Failed ("FOR OUT_FILE COLUMN NOT FOUR");
      else
         Put (File, 'C');
         Set_Col (File, 5);
         if Col (File) /= Four + 1 or Line (File) /= One then
            Failed
              ("FOR OUT_FILE COLUMN UNNECESSARILY " & "CHANGED FROM FOUR");
         else
            Set_Col (File, 8);
            Put (File, "DE");
            Set_Col (File, Two + 1);
            if Col (File) /= Two + One or Line (File) /= Two then
               Failed ("FOR OUT_FILE COLUMN NOT TWO");
            end if;
            Put (File, 'B');
            Set_Col (File, Two);

            if Page (File) /= Two then
               Failed ("PAGE TERMINATOR NOT OUTPUT");
            end if;

            if Line (File) /= One then
               Failed ("LINE TERMINATOR NOT OUTPUT");
            end if;

            if Col (File) /= Two then
               Failed ("COL NOT TWO; IS" & Count'Image (Col (File)));
            end if;

            Put (File, 'X');
         end if;
      end if;

      Check_File (File, "   C   DE#  B#@ X#@%");

      Close (File);

      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH MODE IN_FILE");
            raise Incomplete;
      end;

      Set_Col (File, Four);
      if Col (File) /= Four then
         Failed ("FOR IN_FILE COLUMN NOT FOUR");
      else
         Get (File, Item_Char);
         if Item_Char /= 'C' then
            Failed ("SET_COL FOR READ; ACTUALLY READ '" & Item_Char & "'");
         end if;

         Set_Col (File, 5);
         if Col (File) /= Four + 1 or Line (File) /= One then
            Failed ("FOR IN_FILE COLUMN UNNECESSARILY " & "CHANGED FROM FOUR");
         else
            Set_Col (File, 9);
            Get (File, Item_Char);
            if Item_Char /= 'E' then
               Failed
                 ("SET_COL FOR READ 2; ACTUALLY READ '" & Item_Char & "'");
            end if;

            Set_Col (File, 3);
            Get (File, Item_Char);
            if Item_Char /= 'B' then
               Failed
                 ("SET_COL FOR READ 3; ACTUALLY READ '" & Item_Char & "'");
            end if;

            if Col (File) /= 4 or Line (File) /= Two then
               Failed ("FOR IN_FILE COLUMN NOT TWO");
            end if;
         end if;
      end if;

      begin
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;

   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3409c;
