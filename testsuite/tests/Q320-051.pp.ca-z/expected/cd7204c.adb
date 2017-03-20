-- CD7204C.ADA

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
--     CHECK THAT THE PREFIX OF THE 'POSITION, 'LAST_BIT, AND 'FIRST_BIT
--     ATTRIBUTES CAN DENOTE A RECORD COMPONENT, AND THE ATTRIBUTES
--     RETURN APPROPRIATE VALUES WHEN A RECORD REPRESENTATION CLAUSE
--     IS GIVEN.

-- HISTORY:
--     BCB 09/14/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with System;
with Report; use Report;

procedure Cd7204c is

   Units_Per_Integer : constant :=
     (Integer'Size + System.Storage_Unit - 1) / System.Storage_Unit;

   type Basic_Rec is record
      Check_Int  : Integer;
      Check_Char : Character;
   end record;

   for Basic_Rec use record
      Check_Int  at                     0 range 0 ..   Integer'Size - 1;
      Check_Char at 1 * Units_Per_Integer range 0 .. Character'Size - 1;
   end record;

   Check_Rec : Basic_Rec;

begin

   Test
     ("CD7204C",
      "THE PREFIX OF THE 'POSITION, " &
      "'LAST_BIT, AND 'FIRST_BIT ATTRIBUTES CAN " &
      "DENOTE A RECORD COMPONENT, AND THE ATTRIBUTES " &
      "RETURN APPROPRIATE VALUES WHEN A RECORD " &
      "REPRESENTATION CLAUSE IS GIVEN");

   if Check_Rec.Check_Int'Position /= 0 then
      Failed ("INCORRECT VALUE FOR POSITION OF CHECK_INT");
   end if;

   if Check_Rec.Check_Int'First_Bit /= Ident_Int (0) then
      Failed ("INCORRECT VALUE FOR FIRST_BIT OF CHECK_INT");
   end if;

   if Check_Rec.Check_Int'Last_Bit /= Integer'Size - 1 then
      Failed ("INCORRECT VALUE FOR LAST_BIT OF CHECK_INT");
   end if;

   if Check_Rec.Check_Char'Position /= Ident_Int (Units_Per_Integer) then
      Failed ("INCORRECT VALUE FOR POSITION OF CHECK_CHAR");
   end if;

   if Check_Rec.Check_Char'First_Bit /= 0 then
      Failed ("INCORRECT VALUE FOR FIRST_BIT OF CHECK_CHAR");
   end if;

   if Check_Rec.Check_Char'Last_Bit /= Ident_Int (Character'Size - 1) then
      Failed ("INCORRECT VALUE FOR LAST_BIT OF CHECK_CHAR");
   end if;

   Result;

end Cd7204c;
