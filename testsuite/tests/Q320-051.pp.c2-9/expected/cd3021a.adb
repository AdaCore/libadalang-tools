-- CD3021A.ADA

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
--     CHECK THAT THE AGGREGATE IN AN ENUMERATION REPRESENTATION CLAUSE
--     IS NOT AMBIGUOUS EVEN IF THERE ARE SEVERAL ONE-DIMENSIONAL ARRAY
--     TYPES WITH THE ENUMERATION TYPE AS THE INDEX SUBTYPE.

-- HISTORY:
--     BCB 09/30/87  CREATED ORIGINAL TEST.
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP', CORRECTED
--                    CHECKS FOR FAILURE.

with Report; use Report;

procedure Cd3021a is

   type Enum is (A, B, C);

   type Arr1 is array (Enum) of Integer;
   type Arr2 is array (Enum) of Integer;
   type Arr3 is array (Enum) of Integer;

   for Enum use (A => 1, B => 2, C => 3);

   A1 : Arr1 := (A => 5, B => 6, C => 13);
   A2 : Arr2 := (A => 1, B => 2, C => 3);
   A3 : Arr3 := (A => 0, B => 1, C => 2);

begin

   Test
     ("CD3021A",
      "CHECK THAT THE AGGREGATE IN AN ENUMERATION " &
      "REPRESENTATION CLAUSE IS NOT AMBIGUOUS EVEN " &
      "IF THERE ARE SEVERAL ONE-DIMENSIONAL ARRAY " &
      "TYPES WITH THE ENUMERATION TYPE AS THE INDEX " &
      "SUBTYPE");

   if (A1 /= (Ident_Int (5), Ident_Int (6), Ident_Int (13))) or
     (A2 /= (Ident_Int (1), Ident_Int (2), Ident_Int (3))) or
     (A3 /= (Ident_Int (0), Ident_Int (1), Ident_Int (2)))
   then
      Failed ("INCORRECT VALUES FOR ARRAYS");
   end if;

   Result;
end Cd3021a;
