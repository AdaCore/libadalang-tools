-- CD7204B.ADA

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
--     RETURN APPROPRIATE VALUES WHEN A RECORD REPRESENTATION CLAUSE IS
--     NOT PRESENT.

-- HISTORY:
--     BCB 09/14/87  CREATED ORIGINAL TEST.
--     RJW 02/08/88  REVISED SO THAT TEST PASSES IF BOOLEAN'SIZE = 1.
--     RJW 05/31/90  CORRECTED COMPARISONS INVOLVING SIZES.
--     LDC 10/04/90  ADDED CHECK FOR 'POSITION.

with Report; use Report;

procedure Cd7204b is

   type Basic_Rec is record
      Check_Int  : Integer := 5;
      Check_Bool : Boolean := True;
   end record;

   Check_Rec : Basic_Rec;

begin

   Test
     ("CD7204B",
      "CHECK THAT THE PREFIX OF THE 'POSITION, " &
      "'LAST_BIT, AND 'FIRST_BIT ATTRIBUTES CAN " &
      "DENOTE A RECORD COMPONENT, AND THE ATTRIBUTES " &
      "RETURN APPROPRIATE VALUES WHEN A RECORD " &
      "REPRESENTATION CLAUSE IS NOT PRESENT");

   if Check_Rec.Check_Int'First_Bit >= Check_Rec.Check_Int'Last_Bit then
      Failed ("INCORRECT VALUES FOR FIRST_BIT OR LAST_BIT " & "OF CHECK_INT");
   end if;

   if (Check_Rec.Check_Int'Last_Bit - Check_Rec.Check_Int'First_Bit + 1) <
     Integer'Size
   then
      Failed ("INCORRECT SIZE FOR CHECK_INT");
   end if;

   if Check_Rec.Check_Bool'Position <= Check_Rec.Check_Int'Position then
      Failed ("INCORRECT VALUE FOR 'POSITION OF CHECK_INT " & "OR CHECK_BOOL");
   end if;

   if Check_Rec.Check_Int'Position >= Check_Rec.Check_Bool'Position then
      Failed
        ("INCORRECT VALUE FOR 'POSITION OF CHECK_INT " & "OR CHECK_BOOL - 2");
   end if;

   if Check_Rec.Check_Bool'First_Bit > Check_Rec.Check_Bool'Last_Bit then
      Failed ("INCORRECT VALUE FOR FIRST_BIT OR LAST_BIT " & "OF CHECK_BOOL");
   end if;

   if (Check_Rec.Check_Bool'Last_Bit - Check_Rec.Check_Bool'First_Bit + 1) <
     Boolean'Size
   then
      Failed ("INCORRECT SIZE FOR CHECK_BOOL");
   end if;

   Result;

end Cd7204b;
