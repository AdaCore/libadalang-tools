-- C55B16A.ADA

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
--     CHECK THE PROCESSING OF ITERATIONS OVER AN ENUMERATION TYPE
--     WHOSE (USER-DEFINED) REPRESENTATION CONSISTS OF A NON-CONTIGUOUS
--     SET OF INTEGERS.
--
--     (INHERITANCE (AND SUBSEQUENT OVERRIDING) OF REPRESENTATION
--     SPECIFICATIONS WILL BE TESTED ELSEWHERE.)

-- HISTORY:
--     RM  08/06/82  CREATED ORIGINAL TEST.
--     BCB 01/04/88  MODIFIED HEADER.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;
procedure C55b16a is

   I1 : Integer := 0;

   type Enum is ('A', 'B', 'D', 'C', Z, X, D, A, C);
   for Enum use (-15, -14, -11, -10, 1, 3, 4, 8, 9);

begin

   Test
     ("C55B16A",
      "TEST LOOPING OVER ENUMERATION TYPES WITH" &
      " NON-CONTIGUOUS REPRESENTATION");

   I1 := Ident_Int (0);

   for X in Enum loop

      if X /= Enum'Val (I1) or
        Enum'Pos (X) /= I1                               -- 0..8
      then
         Failed ("LOOP_PARAMETER ASCENDING INCORRECTLY (1)");
      end if;

      I1 := I1 + Ident_Int (1);

   end loop;

   I1 := Ident_Int (6);

   for X in Enum range D .. C loop

      if X /= Enum'Val (I1) or
        Enum'Pos (X) /= I1                               -- 6..8
      then
         Failed ("LOOP_PARAMETER ASCENDING INCORRECTLY (2)");
      end if;

      I1 := I1 + Ident_Int (1);

   end loop;

   I1 := Ident_Int (4);

   for X in reverse 'A' .. Enum'(Z) loop

      if X /= Enum'Val (I1) or
        Enum'Pos (X) /= I1                               -- 4..0
      then
         Failed ("LOOP_PARAMETER DESCENDING INCORRECTLY (3)");
      end if;

      I1 := I1 - Ident_Int (1);

   end loop;

   Result;

end C55b16a;
