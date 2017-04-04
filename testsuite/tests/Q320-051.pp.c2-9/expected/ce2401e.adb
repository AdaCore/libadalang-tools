-- CE2401E.ADA

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
--     FLOATING POINT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY FOR IMPLEMENTATIONS WHICH SUPPORT CREATION OF
--     DIRECT FILES WITH INOUT_FILE MODE AND OPENING OF DIRECT FILES
--     WITH IN_FILE MODE.

-- HISTORY:
--     ABW 08/18/82
--     SPS 09/15/82
--     SPS 11/11/82
--     JBG 05/02/83
--     EG  11/19/85  HANDLE IMPLEMENTATIONS WITH
--                   POSITIVE_COUNT'LAST=1.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/10/87  ISOLATED EXCEPTIONS. SPLIT FIXED POINT TESTS
--                   INTO CE2401I.

with Report; use Report;
with Direct_Io;

procedure Ce2401e is

   End_Subtest : exception;

begin

   Test
     ("CE2401E",
      "CHECK THAT READ, WRITE, SET_INDEX, " &
      "INDEX, SIZE, AND END_OF_FILE ARE " &
      "SUPPORTED FOR DIRECT FILES WITH " &
      "ELEMENT_TYPE FLOAT");

   declare

      package Dir_Flt is new Direct_Io (Float);
      use Dir_Flt;
      File_Flt : File_Type;

   begin
      begin
         Create (File_Flt, Inout_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - FLOAT");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON " & "CREATE - FLOAT");
            raise End_Subtest;
      end;

      declare
         Flt      : Float          := 65.0;
         Item_Flt : Float;
         One_Flt  : Positive_Count := 1;
         Two_Flt  : Positive_Count := 2;
      begin
         begin
            Write (File_Flt, Flt);
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED ON WRITE FOR " & "FLOATING POINT - 1");
         end;

         begin
            Write (File_Flt, Flt, Two_Flt);
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED ON WRITE FOR " & "FLOATING POINT - 2");
         end;

         begin
            if Size (File_Flt) /= Two_Flt then
               Failed ("SIZE FOR FLOATING POINT");
            end if;

            if not End_Of_File (File_Flt) then
               Failed ("WRONG END_OF_FILE VALUE FOR " & "FLOATING POINT");
            end if;

            Set_Index (File_Flt, One_Flt);
            if Index (File_Flt) /= One_Flt then
               Failed ("WRONG INDEX VALUE FOR " & "FLOATING POINT");
            end if;
         end;

         Close (File_Flt);

         begin
            Open (File_Flt, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable ("OPEN FOR IN_FILE " & "MODE NOT SUPPORTED");
               raise End_Subtest;
         end;

         begin
            Read (File_Flt, Item_Flt);
            if Item_Flt /= Flt then
               Failed ("WRONG VALUE READ FOR " & "FLOATING POINT");
            end if;
         exception
            when others =>
               Failed ("READ WITHOUT FROM FOR " & "TYPE FLOATING POINT");
         end;

         begin
            Read (File_Flt, Item_Flt, One_Flt);
            if Item_Flt /= Flt then
               Failed ("WRONG VALUE READ WITH INDEX FOR " & "FLOATING POINT");
            end if;
         exception
            when others =>
               Failed ("READ WITH FROM FOR " & "TYPE FLOATING POINT");
         end;

         begin
            Delete (File_Flt);
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

end Ce2401e;
