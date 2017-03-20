-- CD4051D.ADA

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
--     CHECK THAT A RECORD REPRESENTATION CLAUSE CAN BE GIVEN FOR
--     A DERIVED SUBTYPE WHOSE PARENT TYPE IS A RECORD TYPE WITH
--     VARIANTS AND THE REPRESENTATION CLAUSE MENTIONS COMPONENTS THAT
--     DO NOT EXIST IN THE DERIVED SUBTYPE.

-- HISTORY:
--     RJW 08/25/87  CREATED ORIGINAL TEST.
--     DHH 03/27/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA' AND
--                   ADDED CHECK FOR REPRESENTATION CLAUSE.
--     RJW 10/26/89  REMOVED REFERENCES TO LENGTH_CHECK.
--     THS 09/18/90  MADE CALLS TO IDENT_INT TO DEFEAT OPTIMIZATION.
--     JRL 10/13/96  Adjusted ranges in type definitions to allow 1's
--                   complement machines to represent all values in
--                   the specified number of bits.

with Report; use Report;
with System;
procedure Cd4051d is

   type Int is range -3 .. 3;
   type Large_Int is range -7 .. 7;

   type Basic_Clause (Disc : Boolean) is record
      Bool_Comp : Boolean;
      case Disc is
         when False =>
            Int_Comp : Large_Int;
         when True =>
            Ch_Comp_1 : Int;
            Ch_Comp_2 : Int;
      end case;
   end record;

   type Check_Clause is new Basic_Clause (True);

   for Check_Clause use record
      Disc      at 0 range 0 .. 0;
      Bool_Comp at 0 range 1 .. 1;
      Int_Comp  at 0 range 2 .. 5;
      Ch_Comp_1 at 0 range 2 .. 4;
      Ch_Comp_2 at 0 range 5 .. 7;
   end record;

   Check_Record : Check_Clause := (True, True, -2, -2);

begin
   Test
     ("CD4051D",
      "CHECK THAT A RECORD REPRESENTATION " &
      "CLAUSE CAN BE GIVEN FOR A DERIVED TYPE " &
      "WHOSE PARENT TYPE IS A RECORD TYPE " &
      "WITH VARIANTS AND WHERE THE RECORD " &
      "REPRESENTATION CLAUSE MENTIONS COMPONENTS " &
      "THAT DO NOT EXIST IN THE DERIVED SUBTYPE");

   if Check_Record.Disc'First_Bit /= Ident_Int (0) then
      Failed ("INCORRECT VALUE FOR FIRST_BIT OF DISC");
   end if;

   if Check_Record.Disc'Last_Bit /= Ident_Int (0) then
      Failed ("INCORRECT VALUE FOR LAST_BIT OF DISC");
   end if;

   if Check_Record.Disc'Position /= Ident_Int (0) then
      Failed ("INCORRECT VALUE FOR POSITION OF DISC");
   end if;

   if Check_Record.Bool_Comp'First_Bit /= Ident_Int (1) then
      Failed ("INCORRECT VALUE FOR FIRST_BIT OF BOOL_COMP");
   end if;

   if Check_Record.Bool_Comp'Last_Bit /= Ident_Int (1) then
      Failed ("INCORRECT VALUE FOR LAST_BIT OF BOOL_COMP");
   end if;

   if Check_Record.Bool_Comp'Position /= Ident_Int (0) then
      Failed ("INCORRECT VALUE FOR POSITION OF BOOL_COMP");
   end if;

   if Check_Record.Ch_Comp_1'First_Bit /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR FIRST_BIT OF CH_COMP_1");
   end if;

   if Check_Record.Ch_Comp_1'Last_Bit /= Ident_Int (4) then
      Failed ("INCORRECT VALUE FOR LAST_BIT OF CH_COMP_1");
   end if;

   if Check_Record.Ch_Comp_1'Position /= Ident_Int (0) then
      Failed ("INCORRECT VALUE FOR POSITION OF CH_COMP_1");
   end if;

   if Check_Record.Ch_Comp_2'First_Bit /= Ident_Int (5) then
      Failed ("INCORRECT VALUE FOR FIRST_BIT OF CH_COMP_2");
   end if;

   if Check_Record.Ch_Comp_2'Last_Bit /= Ident_Int (7) then
      Failed ("INCORRECT VALUE FOR LAST_BIT OF CH_COMP_2");
   end if;

   if Check_Record.Ch_Comp_2'Position /= Ident_Int (0) then
      Failed ("INCORRECT VALUE FOR POSITION OF CH_COMP_2");
   end if;

   Result;
end Cd4051d;
