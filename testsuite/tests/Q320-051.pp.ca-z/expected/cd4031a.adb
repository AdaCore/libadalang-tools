-- CD4031A.ADA

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
--     CHECK THAT WHEN A RECORD REPRESENTATION CLAUSE IS GIVEN FOR A
--     VARIANT RECORD TYPE, THEN COMPONENTS BELONGING TO DIFFERENT
--     VARIANTS CAN BE GIVEN OVERLAPPING STORAGE.

-- HISTORY:
--     PWB 07/22/87  CREATED ORIGINAL TEST.
--     DHH 03/27/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA' AND
--                   ADDED CHECK FOR REPRESENTATION CLAUSE.
--     RJW 06/12/90  REMOVED REFERENCES TO LENGTH_CHECK.  REVISED
--                   COMMENTS.
--     JRL 10/13/96  Adjusted ranges in type definitions to allow 1's
--                   complement machines to represent all values in
--                   the specified number of bits.

with Report; use Report;
procedure Cd4031a is

   type Discriminan is range -1 .. 1;
   type Int is range -3 .. 3;
   type Large_Int is range -7 .. 7;

   type Test_Clause (Disc : Discriminan := 0) is record
      case Disc is
         when 0 =>
            Integer_Comp : Large_Int;
         when others =>
            Ch_Comp_1 : Int;
            Ch_Comp_2 : Int;
      end case;
   end record;

   for Test_Clause use record
      Disc         at 0 range 0 .. 1;
      Integer_Comp at 0 range 2 .. 5;
      Ch_Comp_1    at 0 range 2 .. 4;
      Ch_Comp_2    at 0 range 5 .. 7;
   end record;

   type Test_Cl1 is new Test_Clause (Disc => 0);
   type Test_Cl2 is new Test_Clause (Disc => 1);
   Test_Record  : Test_Cl1;
   Test_Record1 : Test_Cl2;

   Integer_Comp_First, Ch_Comp_1_First : Integer;

begin
   Test
     ("CD4031A",
      "IN RECORD REPRESENTATION CLAUSES " &
      "FOR VARIANT RECORD TYPES, " &
      "COMPONENTS OF DIFFERENT VARIANTS " &
      "CAN BE GIVEN OVERLAPPING STORAGE");

   Test_Record        := (0, -7);
   Integer_Comp_First := Test_Record.Integer_Comp'First_Bit;

   Test_Record1    := (1, -3, -3);
   Ch_Comp_1_First := Test_Record1.Ch_Comp_1'First_Bit;

   if Integer_Comp_First /= Ch_Comp_1_First then
      Failed ("COMPONENTS DO NOT BEGIN AT SAME POINT");
   end if;

   Result;
end Cd4031a;
