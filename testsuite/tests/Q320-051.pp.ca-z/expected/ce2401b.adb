-- CE2401B.ADA

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
--     END_OF_FILE FOR DIRECT FILES WITH ELEMENT_TYPES BOOLEAN,
--     ACCESS, AND ENUMERATED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     ABW 08/18/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/07/87  ISOLATED EXCEPTIONS.

with Report; use Report;
with Direct_Io;

procedure Ce2401b is
   End_Subtest : exception;
begin

   Test
     ("CE2401B",
      "CHECK READ, WRITE, SET_INDEX " & "INDEX, SIZE, AND END_OF_FILE FOR " &
      "DIRECT FILES FOR BOOLEAN, ACCESS " & "AND ENUMERATION TYPES");
   declare
      package Dir_Bool is new Direct_Io (Boolean);
      use Dir_Bool;
      File_Bool : File_Type;
   begin
      begin
         Create (File_Bool, Inout_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - BOOLEAN");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON " & "CREATE - BOOLEAN");
            raise End_Subtest;
      end;

      declare
         Bool      : Boolean        := Ident_Bool (True);
         Item_Bool : Boolean;
         One_Bool  : Positive_Count := 1;
         Two_Bool  : Positive_Count := 2;
      begin
         begin
            Write (File_Bool, Bool);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "BOOLEAN - 1");
         end;

         begin
            Write (File_Bool, Bool, Two_Bool);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "BOOLEAN - 2");
         end;

         begin
            if Size (File_Bool) /= Two_Bool then
               Failed ("SIZE FOR TYPE BOOLEAN");
            end if;
            if not End_Of_File (File_Bool) then
               Failed ("WRONG END_OF_FILE VALUE FOR " & "BOOLEAN");
            end if;
            Set_Index (File_Bool, One_Bool);
            if Index (File_Bool) /= One_Bool then
               Failed ("WRONG INDEX VALUE FOR TYPE BOOLEAN");
            end if;
         end;

         Close (File_Bool);

         begin
            Open (File_Bool, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE MODE " & "NOT SUPPORTED - 1");
               raise End_Subtest;
         end;

         begin
            Read (File_Bool, Item_Bool);
            if Item_Bool /= Bool then
               Failed ("INCORRECT BOOLEAN VALUE READ - 1");
            end if;
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR " & "TYPE BOOLEAN");
         end;

         begin
            Read (File_Bool, Item_Bool, One_Bool);
            if Item_Bool /= Bool then
               Failed ("INCORRECT BOOLEAN VALUE READ - 2");
            end if;
         exception
            when others =>
               Failed ("READ WITH FROM FOR BOOLEAN");
         end;
      end;

      begin
         Delete (File_Bool);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   declare
      type Enumerated is (One, Two, Three);
      package Dir_Enum is new Direct_Io (Enumerated);
      use Dir_Enum;
      File_Enum : File_Type;
   begin
      begin
         Create (File_Enum, Inout_File, Legal_File_Name (2));
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - ENUMERATED");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON " & "CREATE - ENUMERATED");
            raise End_Subtest;
      end;

      declare
         Enum      : Enumerated     := (Three);
         Item_Enum : Enumerated;
         One_Enum  : Positive_Count := 1;
         Two_Enum  : Positive_Count := 2;
      begin
         begin
            Write (File_Enum, Enum);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "ENUMERATED - 1");
         end;

         begin
            Write (File_Enum, Enum, Two_Enum);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "ENUMERATED - 2");
         end;

         begin
            if Size (File_Enum) /= Two_Enum then
               Failed ("SIZE FOR TYPE ENUMERATED");
            end if;
            if not End_Of_File (File_Enum) then
               Failed ("WRONG END_OF_FILE VALUE FOR TYPE " & "ENUMERATED");
            end if;
            Set_Index (File_Enum, One_Enum);
            if Index (File_Enum) /= One_Enum then
               Failed ("WRONG INDEX VALUE FOR TYPE " & "ENUMERATED");
            end if;
         end;

         Close (File_Enum);

         begin
            Open (File_Enum, In_File, Legal_File_Name (2));
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE MODE " & "NOT SUPPORTED - 2");
               raise End_Subtest;
         end;

         begin
            Read (File_Enum, Item_Enum);
            if Item_Enum /= Enum then
               Failed ("INCORRECT ENUM VALUE READ - 1");
            end if;
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR ENUMERATED");
         end;

         begin
            Read (File_Enum, Item_Enum, One_Enum);
            if Item_Enum /= Enum then
               Failed ("INCORRECT ENUM VALUE READ - 2");
            end if;
         exception
            when others =>
               Failed ("READ WITH FROM FOR " & "TYPE ENUMERATED");
         end;
      end;

      begin
         Delete (File_Enum);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   declare
      type Acc_Int is access Integer;
      package Dir_Acc is new Direct_Io (Acc_Int);
      use Dir_Acc;
      File_Acc : File_Type;
   begin
      begin
         Create (File_Acc, Inout_File, Legal_File_Name (3));
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - ACCESS");
            raise End_Subtest;
      end;

      declare
         Acc      : Acc_Int        := new Integer'(33);
         Item_Acc : Acc_Int;
         One_Acc  : Positive_Count := 1;
         Two_Acc  : Positive_Count := 2;
      begin
         begin
            Write (File_Acc, Acc);
         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "ACCESS - 1");
         end;

         begin
            Write (File_Acc, Acc, Two_Acc);

         exception
            when others =>
               Failed ("EXCEPTION RAISED ON WRITE FOR " & "ACCESS - 2");
         end;

         begin
            if Size (File_Acc) /= Two_Acc then
               Failed ("SIZE FOR TYPE ACCESS");
            end if;
            if not End_Of_File (File_Acc) then
               Failed ("WRONG END_OF_FILE VALUE FOR ACCESS");
            end if;
            Set_Index (File_Acc, One_Acc);
            if Index (File_Acc) /= One_Acc then
               Failed ("WRONG INDEX VALUE FOR TYPE ACCESS");
            end if;
         end;

         Close (File_Acc);

         begin
            Open (File_Acc, In_File, Legal_File_Name (3));
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE NOT " & "SUPPORTED - 3");
               raise End_Subtest;
         end;

         begin
            Read (File_Acc, Item_Acc);
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR ACCESS");
         end;

         begin
            Read (File_Acc, Item_Acc, One_Acc);
         exception
            when others =>
               Failed ("READ WITH FROM FOR ACCESS");
         end;
      end;

      begin
         Delete (File_Acc);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   Result;

end Ce2401b;
