-- C55B03A.ADA

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
-- CHECK THAT THE LOOP_PARAMETER IS ASSIGNED VALUES IN ASCENDING ORDER
--   IF REVERSE IS ABSENT, AND DESCENDING ORDER IF REVERSE IS PRESENT.

-- DAS 1/12/81
-- SPS 3/2/83

with Report;
procedure C55b03a is

   use Report;
   I1 : Integer;

begin
   Test
     ("C55B03A",
      "CHECK CORRECT ORDER OF VALUE SEQUENCING" & " FOR A LOOP_PARAMETER");

   I1 := 0;
   for I in Ident_Int (1) .. Ident_Int (5) loop
      I1 := I1 + 1;
      if (I /= I1) then
         Failed ("LOOP_PARAMETER ASCENDING INCORRECTLY");
      end if;
   end loop;

   I1 := 6;
   for I in reverse Ident_Int (1) .. Ident_Int (5) loop
      I1 := I1 - 1;
      if (I /= I1) then
         Failed ("LOOP_PARAMETER DESCENDING INCORRECTLY");
      end if;
   end loop;

   Result;

end C55b03a;
