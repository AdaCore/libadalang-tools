-- CE3410C.ADA

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
--     CHECK THAT SET_LINE SETS THE CURRENT LINE NUMBER TO THE VALUE
--     SPECIFIED BY TO FOR FILES OF MODES IN_FILE AND OUT_FILE.
--     CHECK THAT IT HAS NO EFFECT IF THE VALUE SPECIFIED BY TO IS
--     EQUAL TO THE CURRENT LINE NUMBER FOR BOTH IN_FILE AND OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/20/82
--     JBG 01/27/83
--     EG  05/22/85
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/01/87  REMOVED DEPENDENCE ON RESET, ADDED MORE TEST
--                   CASES, AND CHECKED FOR USE_ERROR ON DELETE.
--     JRL 02/29/96  Added File parameter to call to Set_Page_Length.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3410c is

   Incomplete : exception;

begin
   Test ("CE3410C", "CHECK THAT SET_LINE SETS LINE " & "NUMBER CORRECTLY");

   declare
      File      : File_Type;
      Char      : Character      := ('C');
      Item_Char : Character;
      One       : Positive_Count := Positive_Count (Ident_Int (1));
      Two       : Positive_Count := Positive_Count (Ident_Int (2));
      Three     : Positive_Count := Positive_Count (Ident_Int (3));
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

      Set_Line (File, Four);
      if Line (File) /= Four then
         Failed ("FOR OUT_FILE LINE NOT FOUR");
      else
         Put (File, 'C');
         New_Line (File);
         Set_Line (File, 5);
         if Line (File) /= Four + 1 then
            Failed ("FOR OUT_FILE LINE UNNECESSARILY " & "CHANGED FROM FOUR");
         else
            Set_Line (File, 8);
            Put (File, "DE");
            Set_Line (File, Two + 1);
            if Line (File) /= Two + One then
               Failed ("FOR OUT_FILE LINE NOT THREE");
            end if;

            Set_Line (File, Two);

            if Page (File) /= One + Two then
               Failed ("PAGE TERMINATOR NOT OUTPUT - 2");
            end if;

            if Line (File) /= Two then
               Failed ("LINE NOT TWO; IS" & Count'Image (Line (File)));
            end if;

            Set_Page_Length (File, Two);
            Put (File, 'X');
            Set_Line (File, Two);
            Put (File, 'Y');

            if Line (File) /= Two then
               Failed ("LINE NOT TWO; IS " & Count'Image (Line (File)));
            end if;

            if Page (File) /= Three then
               Failed ("PAGE NOT THREE; IS " & Count'Image (Page (File)));
            end if;

         end if;
      end if;

      Check_File (File, "###C####DE#@##@#XY#@%");

      Close (File);

      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED FOR TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      Set_Line (File, Four);
      if Line (File) /= Four then
         Failed ("FOR IN_FILE LINE NOT FOUR");
      else
         Get (File, Item_Char);
         if Item_Char /= 'C' then
            Failed ("SET_LINE FOR READ; ACTUALLY READ '" & Item_Char & "'");
         end if;

         Skip_Line (File);
         Set_Line (File, 5);
         if Line (File) /= Four + 1 or Page (File) /= One then
            Failed ("INCORRECT LINE OR PAGE");
         else
            Set_Line (File, 8);
            Get (File, Item_Char);
            if Item_Char /= 'D' then
               Failed
                 ("SET_LINE FOR READ 2; ACTUALLY READ '" & Item_Char & "'");
            end if;

            Set_Line (File, Two);
            if Page (File) /= Two then
               Failed ("FOR IN_FILE PAGE NOT TWO");
            end if;

            Set_Line (File, Two);
            if Page (File) /= Two or Line (File) /= Two then
               Failed ("FOR IN_FILE PAGE NOT 2");
            end if;

            Skip_Line (File);
            Set_Line (File, Two);

            Get (File, Item_Char);

            if Item_Char /= 'X' then
               Failed
                 ("SET_LINE FOR READ 3; ACTUALLY READ '" & Item_Char & "'");
            end if;

         end if;
      end if;

      begin
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

end Ce3410c;
