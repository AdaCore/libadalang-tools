-- CE2401H.ADA

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
--     CHECK THAT READ, WRITE, SET_INDEX, INDEX, SIZE, AND
--     END_OF_FILE ARE SUPPORTED FOR DIRECT FILES WITH
--     ELEMENT_TYPE UNCONSTRAINED RECORDS WITH DEFAULT DISCRIMINANTS.

--     THIS INSTANTIATION IS ALWAYS LEGAL BY AI-00037.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH INOUT_FILE MODE AND OPENING WITH IN_FILE MODE FOR
--     DIRECT FILES.

-- HISTORY:
--     TBN 05/15/86
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/10/87  ISOLATED EXCEPTIONS.

with Report; use Report;
with Direct_Io;

procedure Ce2401h is

   End_Subtest : exception;

begin

   Test
     ("CE2401H",
      "CHECK THAT READ, WRITE, SET_INDEX, INDEX, " &
      "SIZE, AND END_OF_FILE ARE SUPPORTED FOR " &
      "DIRECT FILES WITH ELEMENT_TYPE UNCONSTRAINED " &
      "RECORDS WITH DEFAULT DISCRIMINANTS");

   declare
      type Rec_Def (Discr : Integer := 1) is record
         One   : Integer := Discr;
         Two   : Integer := 3;
         Three : Integer := 5;
         Four  : Integer := 7;
      end record;
      package Dir_Rec_Def is new Direct_Io (Rec_Def);
      use Dir_Rec_Def;
      File1 : File_Type;
      Rec   : Rec_Def;
      Item  : Rec_Def;
      One   : Positive_Count := 1;
      Two   : Positive_Count := 2;

   begin
      begin
         Create (File1, Inout_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("CREATE WITH INOUT_FILE MODE " & "NOT SUPPORTED FOR " &
               "UNCONSTRAINED RECORDS WITH " & "DEFAULT DISCRIMINATES");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON DIRECT " & "CREATE");
            raise End_Subtest;
      end;

      begin
         Write (File1, Rec);
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED ON WRITE FOR " & "RECORD WITH DEFAULT - 1");
      end;

      begin
         Write (File1, Rec, Two);
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED ON WRITE FOR " & "RECORD WITH DEFAULT - 2");
      end;

      begin
         if Size (File1) /= Two then
            Failed ("SIZE FOR RECORD WITH DEFAULT");
         end if;
         if not End_Of_File (File1) then
            Failed
              ("WRONG END_OF_FILE VALUE FOR TYPE " & "RECORD WITH DEFAULT");
         end if;
         Set_Index (File1, One);
         if Index (File1) /= One then
            Failed ("WRONG INDEX VALUE FOR RECORD" & "WITH DEFAULT");
         end if;
      end;

      Close (File1);

      begin
         Open (File1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("OPEN FOR IN_FILE NOT SUPPORTED");
            raise End_Subtest;
      end;

      begin
         Read (File1, Item);
         if Item /= (1, 1, 3, 5, 7) then
            Failed ("WRONG VALUE READ");
         end if;
      exception
         when others =>
            Failed ("READ WITHOUT FROM FOR " & "TYPE RECORD WITH DEFAULT");
      end;

      begin
         Item := (others => 0);
         Read (File1, Item, One);
         if Item /= (1, 1, 3, 5, 7) then
            Failed ("WRONG VALUE READ");
         end if;
      exception
         when others =>
            Failed ("READ WITH FROM FOR " & "TYPE RECORD WITH DEFAULT");
      end;

      begin
         Delete (File1);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when End_Subtest =>
         null;
   end;

   Result;

end Ce2401h;
