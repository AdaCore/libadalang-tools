-- CE2401A.ADA

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
--     AND WITHOUT PARAMETER TO), SET_INDEX, INDEX, SIZE AND
--     END_OF_FILE ARE SUPPORTED FOR DIRECT FILES WITH ELEMENT_TYPES
--     STRING, CHARACTER, AND INTEGER.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT DIRECT FILES.

-- HISTORY:
--     ABW 08/16/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 07/31/87  ISOLATED EXCEPTIONS.

with Report; use Report;
with Direct_Io;

procedure Ce2401a is
   End_Subtest : exception;
begin

   Test
     ("CE2401A",
      "CHECK THAT READ, WRITE, SET_INDEX " &
      "INDEX, SIZE AND END_OF_FILE ARE " & "SUPPORTED FOR DIRECT FILES");

   declare
      subtype Str_Type is String (1 .. 12);
      package Dir_Str is new Direct_Io (Str_Type);
      use Dir_Str;
      File_Str : File_Type;
   begin
      begin
         Create (File_Str, Inout_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - STRING");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON " & "CREATE - STRING");
            raise End_Subtest;
      end;

      declare
         Str      : Str_Type       := "TEXT OF FILE";
         Item_Str : Str_Type;
         One_Str  : Positive_Count := 1;
         Two_Str  : Positive_Count := 2;
      begin
         begin
            Write (File_Str, Str);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "STRING - 1");
         end;

         begin
            Write (File_Str, Str, Two_Str);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "STRING - 2");
         end;

         begin
            if Size (File_Str) /= Two_Str then
               Failed ("SIZE FOR TYPE STRING");
            end if;
            if not End_Of_File (File_Str) then
               Failed ("WRONG END_OF_FILE VALUE FOR STRING");
            end if;
            Set_Index (File_Str, One_Str);
            if Index (File_Str) /= One_Str then
               Failed ("WRONG INDEX VALUE FOR STRING");
            end if;
         end;

         Close (File_Str);

         begin
            Open (File_Str, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE MODE " & "NOT SUPPORTED - 1");
               raise End_Subtest;
         end;

         begin
            Read (File_Str, Item_Str);
            if Item_Str /= Str then
               Failed ("INCORRECT STRING VALUE READ - 1");
            end if;
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR STRING");
         end;

         begin
            Read (File_Str, Item_Str, One_Str);
            if Item_Str /= Str then
               Failed ("INCORRECT STRING VALUE READ - 2");
            end if;
         exception
            when others =>
               Failed ("READ WITH FROM FOR STRING");
         end;
      end;

      begin
         Delete (File_Str);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   declare
      package Dir_Chr is new Direct_Io (Character);
      use Dir_Chr;
      File_Chr : File_Type;
   begin
      begin
         Create (File_Chr, Inout_File, Legal_File_Name (2));
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - CHARACTER");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON " & "CREATE - CHARACTER");
            raise End_Subtest;
      end;

      declare
         Chr      : Character      := 'C';
         Item_Chr : Character;
         One_Chr  : Positive_Count := 1;
         Two_Chr  : Positive_Count := 2;
      begin
         begin
            Write (File_Chr, Chr);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "CHARACTER - 1");
         end;

         begin
            Write (File_Chr, Chr, Two_Chr);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "CHARACTER - 2");
         end;

         begin
            if Size (File_Chr) /= Two_Chr then
               Failed ("SIZE FOR TYPE CHARACTER");
            end if;
            if not End_Of_File (File_Chr) then
               Failed ("WRONG END_OF_FILE VALUE FOR TYPE " & "CHARACTER");
            end if;
            Set_Index (File_Chr, One_Chr);
            if Index (File_Chr) /= One_Chr then
               Failed ("WRONG INDEX VALUE FOR TYPE " & "CHARACTER");
            end if;
         end;

         Close (File_Chr);

         begin
            Open (File_Chr, In_File, Legal_File_Name (2));
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE MODE " & "NOT SUPPORTED - 2");
               raise End_Subtest;
         end;

         begin
            Read (File_Chr, Item_Chr);
            if Item_Chr /= Chr then
               Failed ("INCORRECT CHR VALUE READ - 1");
            end if;
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR " & "TYPE CHARACTER");
         end;

         begin
            Read (File_Chr, Item_Chr, One_Chr);
            if Item_Chr /= Chr then
               Failed ("INCORRECT CHR VALUE READ - 2");
            end if;
         exception
            when others =>
               Failed ("READ WITH FROM FOR " & "TYPE CHARACTER");
         end;
      end;

      begin
         Delete (File_Chr);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   declare
      package Dir_Int is new Direct_Io (Integer);
      use Dir_Int;
      File_Int : File_Type;
   begin
      begin
         Create (File_Int, Inout_File, Legal_File_Name (3));
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - INTEGER");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON " & "CREATE - INTEGER");
            raise End_Subtest;
      end;

      declare
         Int      : Integer        := Ident_Int (33);
         Item_Int : Integer;
         One_Int  : Positive_Count := 1;
         Two_Int  : Positive_Count := 2;
      begin
         begin
            Write (File_Int, Int);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "INTEGER - 1");
         end;

         begin
            Write (File_Int, Int, Two_Int);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "INTEGER - 2");
         end;

         begin
            if Size (File_Int) /= Two_Int then
               Failed ("SIZE FOR TYPE INTEGER");
            end if;
            if not End_Of_File (File_Int) then
               Failed ("WRONG END_OF_FILE VALUE FOR TYPE " & "INTEGER");
            end if;
            Set_Index (File_Int, One_Int);
            if Index (File_Int) /= One_Int then
               Failed ("WRONG INDEX VALUE FOR TYPE INTEGER");
            end if;
         end;

         Close (File_Int);

         begin
            Open (File_Int, In_File, Legal_File_Name (3));
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE MODE " & "NOT SUPPORTED - 3");
               raise End_Subtest;
         end;

         begin
            Read (File_Int, Item_Int);
            if Item_Int /= Int then
               Failed ("INCORRECT INT VALUE READ - 1");
            end if;
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR " & "TYPE INTEGER");
         end;

         begin
            Read (File_Int, Item_Int, One_Int);
            if Item_Int /= Int then
               Failed ("INCORRECT INT VALUE READ - 2");
            end if;
         exception
            when others =>
               Failed ("READ WITH FROM FOR " & "TYPE INTEGER");
         end;
      end;

      begin
         Delete (File_Int);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   Result;

end Ce2401a;
