-- CD4051B.ADA

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
--     CHECK THAT A RECORD REPRESENTATION CLAUSE WHICH CHANGES THE
--     ORDER OF THE COMPONENT STORAGE CAN BE GIVEN FOR A DERIVED TYPE
--     WHOSE PARENT TYPE IS A RECORD WITHOUT A DISCRIMINANT.

-- HISTORY:
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP'.
--     RJW 08/25/87  CREATED ORIGINAL TEST.

with Report; use Report;
with System;
procedure Cd4051b is

   Units_Per_Integer : constant :=
     (Integer'Size + System.Storage_Unit - 1) / System.Storage_Unit;

   type Basic_Clause is record
      Int_Comp  : Integer;
      Char_Comp : Character;
   end record;

   type Check_Clause is new Basic_Clause;

   for Check_Clause use record
      Int_Comp  at 1 * Units_Per_Integer range 0 ..   Integer'Size - 1;
      Char_Comp at                     0 range 0 .. Character'Size - 1;
   end record;

   Check_Record : Check_Clause := (1, 'A');

begin
   Test
     ("CD4051B",
      "CHECK THAT A RECORD REPRESENTATION " &
      "CLAUSE WHICH CHANGES THE ORDER OF COMPONENT " &
      "STORAGE CAN BE GIVEN FOR A DERIVED TYPE " &
      "WHOSE PARENT TYPE IS IS A RECORD TYPE " &
      "WITHOUT DISCRIMINANTS");

   if Check_Record.Int_Comp'First_Bit /= 0 then
      Failed ("INCORRECT VALUE FOR FIRST_BIT OF INT_COMP");
   end if;

   if Check_Record.Int_Comp'Last_Bit /= Integer'Size - 1 then
      Failed ("INCORRECT VALUE FOR LAST_BIT OF INT_COMP");
   end if;

   if Check_Record.Int_Comp'Position /= Ident_Int (Units_Per_Integer) then
      Failed ("INCORRECT VALUE FOR POSITION OF INT_COMP");
   end if;

   if Check_Record.Char_Comp'First_Bit /= Ident_Int (0) then
      Failed ("INCORRECT VALUE FOR FIRST_BIT OF CHAR_COMP");
   end if;

   if Check_Record.Char_Comp'Last_Bit /= Ident_Int (Character'Size - 1) then
      Failed ("INCORRECT VALUE FOR LAST_BIT OF CHAR_COMP");
   end if;

   if Check_Record.Char_Comp'Position /= Ident_Int (0) then
      Failed ("INCORRECT VALUE FOR POSITION OF CHAR_COMP");
   end if;

   Result;
end Cd4051b;
