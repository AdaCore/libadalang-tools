-- CE2401C.ADA

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
--     CHECK READ (WITH AND WITHOUT PARAMETER FROM), WRITE (WITH
--     AND WITHOUT PARAMETER TO), SET_INDEX, INDEX, SIZE, AND
--     END_OF_FILE ARE IMPLEMENTED FOR DIRECT FILES WITH
--     ELEMENT_TYPE CONSTRAINED ARRAY, AND RECORD WITHOUT DISCRIMINANTS.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     ABW 08/18/82
--     SPS 09/20/82
--     SPS 11/09/82
--     JBG 05/02/83
--     JRK 03/26/84
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/10/87  ISOLATED EXCEPTIONS.

with Report; use Report;
with Direct_Io;

procedure Ce2401c is
   End_Subtest : exception;
begin

   Test
     ("CE2401C",
      "CHECK READ, WRITE, SET_INDEX " &
      "INDEX, SIZE, AND END_OF_FILE FOR " &
      "DIRECT FILES FOR CONSTRAINED ARRAY TYPES, " &
      "AND RECORD TYPES WITHOUT DISCRIMINANTS");

   declare
      type Arr_Cn is array (1 .. 5) of Boolean;
      package Dir_Arr_Cn is new Direct_Io (Arr_Cn);
      use Dir_Arr_Cn;
      File : File_Type;
   begin
      begin
         Create (File, Inout_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " &
               "ON CREATE - CONSTRAINED ARRAY");
            raise End_Subtest;
         when others =>
            Failed
              ("UNEXPECTED ERROR RAISED ON " & "CREATE - CONSTRAINED ARRAY");
            raise End_Subtest;
      end;

      declare
         Arr  : Arr_Cn         := (True, True, False, True, True);
         Item : Arr_Cn;
         One  : Positive_Count := 1;
         Two  : Positive_Count := 2;
      begin
         begin
            Write (File, Arr);
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED ON WRITE FOR " & "CONTRAINED ARRAY - 1");
         end;

         begin
            Write (File, Arr, Two);
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED ON WRITE FOR " & "CONSTRAINED ARRAY - 2");
         end;

         begin
            if Size (File) /= Two then
               Failed ("SIZE FOR TYPE CONSTRAINED ARRAY");
            end if;
            if not End_Of_File (File) then
               Failed
                 ("WRONG END_OF_FILE VALUE FOR TYPE " & "CONSTRAINED ARRAY");
            end if;
            Set_Index (File, One);
            if Index (File) /= One then
               Failed ("WRONG INDEX VALUE FOR TYPE " & "CONSTRAINED ARRAY");
            end if;
         end;

         Close (File);

         begin
            Open (File, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE MODE " & "NOT SUPPORTED - 1");
               raise End_Subtest;
         end;

         begin
            Read (File, Item);
            if Item /= Arr then
               Failed ("INCORRECT ARRAY VALUES READ " & "- 1");
            end if;
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR " & "TYPE CONSTRAINED ARRAY");
         end;

         begin
            Read (File, Item, One);
            if Item /= Arr then
               Failed ("INCORRECT ARRAY VALUES READ " & "- 2");
            end if;
         exception
            when others =>
               Failed ("READ WITH FROM FOR " & "TYPE CONSTRAINED ARRAY");
         end;
      end;

      begin
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   declare
      type Rec is record
         One : Integer;
         Two : Integer;
      end record;
      package Dir_Rec is new Direct_Io (Rec);
      use Dir_Rec;
      File : File_Type;
   begin
      begin
         Create (File, Inout_File, Legal_File_Name (2));
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - RECORD");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON CREATE - " & "RECORD");
      end;

      declare
         Rec1 : Rec            := Rec'(One => 18, Two => 36);
         Item : Rec;
         One  : Positive_Count := 1;
         Two  : Positive_Count := 2;
      begin
         begin
            Write (File, Rec1);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR - " & "RECORD - 1");
         end;

         begin
            Write (File, Rec1, Two);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR - " & "RECORD - 2");
         end;

         begin
            if Size (File) /= Two then
               Failed ("SIZE FOR TYPE RECORD");
            end if;
            if not End_Of_File (File) then
               Failed ("WRONG END_OF_FILE VALUE FOR RECORD");
            end if;
            Set_Index (File, One);
            if Index (File) /= One then
               Failed ("WRONG INDEX VALUE FOR TYPE RECORD");
            end if;
         end;

         Close (File);

         begin
            Open (File, In_File, Legal_File_Name (2));
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE MODE " & "NOT SUPPORTED - 2");
               raise End_Subtest;
         end;

         begin
            Read (File, Item);
            if Item /= Rec1 then
               Failed ("INCORRECT RECORD VALUES READ " & "- 1");
            end if;
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR RECORD");
         end;

         begin
            Read (File, Item, One);
            if Item /= Rec1 then
               Failed ("INCORRECT RECORD VALUES READ " & "- 2");
            end if;
         exception
            when others =>
               Failed ("READ WITH FROM FOR " & "TYPE RECORD");
         end;
      end;

      begin
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   Result;

end Ce2401c;
