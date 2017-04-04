-- CE3905L.ADA

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
--     CHECK THAT DATA_ERROR IS RAISED, BY GET, WHEN THE INPUT CONTAINS
--
--     1. EMBEDDED BLANKS.
--     2. SINGLY QUOTED CHARACTER LITERALS.
--     3. IDENTIFIERS BEGINNING WITH NON LETTERS.
--     4. IDENTIFIERS CONTAINING SPECIAL CHARACTERS.
--     5. CONSECUTIVE UNDERSCORES.
--     6. LEADING OR TRAILING UNDERSCORES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     VKG 02/14/83
--     SPS 03/16/83
--     CPP 07/30/84
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/16/87  REMOVED UNNECESSARY CODE AND CORRECTED
--                   EXCEPTION HANDLING.

with Text_Io; use Text_Io;
with Report;  use Report;

procedure Ce3905l is

   Incomplete : exception;

begin
   Test ("CE3905L", "CHECK GET FOR ENUMERATION_IO " & "WITH LEXICAL ERRORS");
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

      Put (Ft, "RED ISH");
      New_Line (Ft);
      Put (Ft, "'A ");
      New_Line (Ft);
      Put (Ft, "2REDISH");
      New_Line (Ft);
      Put (Ft, "BLUE$%ISH");
      New_Line (Ft);
      Put (Ft, "RED__ISH");
      New_Line (Ft);
      Put (Ft, "_YELLOWISH");
      New_Line (Ft);
      Put (Ft, "GREENISH_");
      New_Line (Ft);

      Close (Ft);

      declare
         type Colour is (Greyish, Redish, Blueish, Yellowish, Greenish, 'A');
         package Colour_Io is new Enumeration_Io (Colour);
         use Colour_Io;
         X  : Colour := Greyish;
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
            Failed ("DATA_ERROR NOT RAISED - 1");
         exception
            when Data_Error =>
               if X /= Greyish then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - 1");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 1");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - 1");
         else
            Get (Ft, Ch);
            if Ch /= ' ' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- 1: CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - 2");
         exception
            when Data_Error =>
               if X /= Greyish then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - 2");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 2");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - 2");
         else
            Get (Ft, Ch);
            if Ch /= ' ' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- 2: CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - 3");
         exception
            when Data_Error =>
               if X /= Greyish then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - 3");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 3");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - 3");
         else
            Get (Ft, Ch);
            if Ch /= '2' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- 3: CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - 4");
         exception
            when Data_Error =>
               if X /= Greyish then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - 4");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 4");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - 4");
         else
            Get (Ft, Ch);
            if Ch /= '$' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- 4: CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - 5");
         exception
            when Data_Error =>
               if X /= Greyish then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - 5");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 5");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - 5");
         else
            Get (Ft, Ch);
            if Ch /= '_' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- 5: CHAR IS " & Ch);
            else
               Get (Ft, Ch);
               if Ch /= 'I' then
                  Failed ("ERROR READING DATA - 5");
               end if;
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - 6");
         exception
            when Data_Error =>
               if X /= Greyish then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - 6");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 6");
         end;

         if End_Of_Line (Ft) then
            Failed ("GET STOPPED AT END OF LINE - 6");
         else
            Get (Ft, Ch);
            if Ch /= '_' then
               Failed
                 ("GET STOPPED AT WRONG POSITION " & "- 6: CHAR IS " & Ch);
            end if;
         end if;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - 7");
         exception
            when Data_Error =>
               if X /= Greyish then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - 7");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 7");
         end;

         if not End_Of_Line (Ft) then
            begin
               Get (Ft, X);
               Failed ("GET STOPPED AT WRONG  POSITION " & "- 7");
            exception
               when End_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED FOR " & "EMPTY FILE - 7");
            end;
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

end Ce3905l;
