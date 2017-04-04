-- C45114B.ADA

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
-- CHECK THAT LOGICAL OPERATORS ARE DEFINED FOR PACKED BOOLEAN ARRAYS.

--  RJW  1/17/86

with Report; use Report;
procedure C45114b is

begin

   Test
     ("C45114B",
      "CHECK THAT LOGICAL OPERATORS ARE DEFINED " &
      "FOR PACKED BOOLEAN ARRAYS");

   declare

      type Arr is array (1 .. 32) of Boolean;

      pragma Pack (Arr);

      A : Arr := (True, True, False, False, others => True);
      B : Arr := (True, False, True, False, others => False);

      A_And_B : Arr := (True, others => False);
      A_Or_B  : Arr := Arr'(4 => False, others => True);
      A_Xor_B : Arr := Arr'(1 | 4 => False, others => True);
      Not_A   : Arr := Arr'(3 | 4 => True, others => False);

   begin

      if (A and B) /= A_And_B then
         Failed ("'AND' NOT CORRECTLY DEFINED");
      end if;

      if (A or B) /= A_Or_B then
         Failed ("'OR' NOT CORRECTLY DEFINED");
      end if;

      if (A xor B) /= A_Xor_B then
         Failed ("'XOR' NOT CORRECTLY DEFINED");
      end if;

      if not A /= Not_A then
         Failed ("'NOT' NOT CORRECTLY DEFINED");
      end if;

   end;

   Result;

end C45114b;
