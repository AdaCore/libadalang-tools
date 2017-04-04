-- C83B02A.ADA

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
-- CHECK THAT NESTED LOOPS CAN HAVE IDENTICALLY NAMED PARAMETERS,
--    AND REFERENCES IN THE INNERMOST LOOP ARE ASSOCIATED WITH THE
--    INNERMOST PARAMETER, ETC.

--    RM     4 JUNE 1980

with Report;
procedure C83b02a is

   use Report;

   I, J, K : Integer := 1;

begin

   Test
     ("C83B02A",
      "CHECK THAT NESTED LOOPS CAN HAVE IDENTICALLY NAMED" & " PARAMETERS");

   --   I    J    K
   for Loop_Par in 2 .. 2 loop
      I := I * Loop_Par;                           --   2    1    1
      for Loop_Par in 3 .. 3 loop
         I := I * Loop_Par;                      --   6    1    1
         for Loop_Par in 5 .. 5 loop
            I := I * Loop_Par;                 --  30    1    1
            for Second_Loop_Par in 7 .. 7 loop
               J := J * Second_Loop_Par;     --  30    7    1
               for Second_Loop_Par in 11 .. 11 loop
                  J := J * Second_Loop_Par;--  30   77    1
                  for Second_Loop_Par in 13 .. 13 loop
                     J := J * Second_Loop_Par;--  30 1001    1
                  end loop;
                  K := K * Loop_Par;       --  30 1001    5
               end loop;
               K := K * Loop_Par;            --  30 1001   25
            end loop;
            K := K * Loop_Par;                 --  30 1001  125
         end loop;
         K := K * Loop_Par;                      --  30 1001  375
      end loop;
      K := K * Loop_Par;                           --  30 1001  750
   end loop;

   if I /= 30 or J /= 1_001 or K /= 750 then
      Failed
        ("DID NOT ACCESS INNERMOST ENCLOSING IDENTICALLY " &
         "NAMED LOOP PARAMETER IN NESTED LOOPS");
   end if;

   Result;

end C83b02a;
