-- CD2B11F.ADA

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
--     CHECK THAT IF A COLLECTION SIZE SPECIFICATION IS GIVEN FOR AN
--     ACCESS TYPE WHOSE DESIGNATED TYPE IS A DISCRIMINATED RECORD, THEN
--     OPERATIONS ON VALUES OF THE ACCESS TYPE ARE NOT AFFECTED.

-- HISTORY:
--     BCB 09/29/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;

procedure Cd2b11f is

   Basic_Size : constant := 1_024;

   type Record_Type (Disc : Integer := 100) is record
      Comp1 : Integer;
      Comp2 : Integer;
      Comp3 : Integer;
   end record;

   type Acc_Record is access Record_Type;
   for Acc_Record'Storage_Size use Basic_Size;

   Check_Record1 : Acc_Record;
   Check_Record2 : Acc_Record;

begin

   Test
     ("CD2B11F",
      "CHECK THAT IF A COLLECTION SIZE SPECIFICATION " &
      "IS GIVEN FOR AN ACCESS TYPE WHOSE " &
      "DESIGNATED TYPE IS A DISCRIMINATED RECORD, " &
      "THEN OPERATIONS ON VALUES OF THE ACCESS TYPE " & "ARE NOT AFFECTED");

   Check_Record1       := new Record_Type;
   Check_Record1.Comp1 := 25;
   Check_Record1.Comp2 := 25;
   Check_Record1.Comp3 := 150;

   if Acc_Record'Storage_Size < Basic_Size then
      Failed ("INCORRECT VALUE FOR RECORD TYPE ACCESS " & "STORAGE_SIZE");
   end if;

   if Check_Record1.Disc /= Ident_Int (100) then
      Failed ("INCORRECT VALUE FOR RECORD DISCRIMINANT");
   end if;

   if
     ((Check_Record1.Comp1 /= Check_Record1.Comp2) or
      (Check_Record1.Comp1 = Check_Record1.Comp3))
   then
      Failed ("INCORRECT VALUE FOR RECORD COMPONENT");
   end if;

   if Equal (3, 3) then
      Check_Record2 := Check_Record1;
   end if;

   if Check_Record2 /= Check_Record1 then
      Failed ("INCORRECT RESULTS FOR RELATIONAL OPERATOR");
   end if;

   Result;
end Cd2b11f;
