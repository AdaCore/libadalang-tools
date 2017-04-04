-- C83B02B.ADA

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
-- CHECK THAT NON-NESTED LOOPS CAN HAVE IDENTICALLY NAMED PARAMETERS,
--    AND REFERENCES IN EACH LOOP ARE ASSOCIATED WITH THAT LOOP'S
--    LOOP PARAMETER.  (THIS IS PART  B  OF THE OBJECTIVE.)
-- CHECK ALSO THAT A LOOP PARAMETER CAN HAVE THE SAME IDENTIFIER
--    AS A VARIABLE DECLARED IN THE SCOPE IMMEDIATELY CONTAINING
--    THE LOOP.  (THIS IS PART  C  OF THE OBJECTIVE.)

--    RM     6 JUNE 1980

with Report;
procedure C83b02b is

   use Report;

   I, J : Integer := 1;

begin

   Test
     ("C83B02B",
      "CHECK THAT NON-NESTED LOOPS CAN HAVE IDENTICALLY NAMED" &
      " PARAMETERS");

   Comment
     ("THE NAME MAY BE THE SAME AS THAT OF A VARIABLE" &
      " KNOWN OUTSIDE THE LOOP");

   -- CHECK PART B OF THE OBJECTIVE
   declare
      type Weekday is (Mon, Tue, Wed, Thu, Fri);
   begin

      for Loop_Par in 3 .. 3 loop
         I := I * Loop_Par;              --    3
      end loop;

      for Loop_Par in Fri .. Fri loop
         I := I * Weekday'Pos (Loop_Par); --   12
      end loop;

      for Loop_Par in 7 .. 7 loop
         I := I * Loop_Par;              --  84
      end loop;

   end;

   if I /= 84 then
      Failed
        ("DID NOT ACCESS ENCLOSING IDENTICALLY NAMED " &
         "LOOP PARAMETER IN NON-NESTED LOOPS");
   end if;

   -- CHECK PART C OF THE OBJECTIVE
   declare
      Loop_Par : Integer := 2;
   begin

      J := J * Loop_Par;                          --    2

      for Loop_Par in 3 .. 3 loop
         J := J * Loop_Par;                     --    6
      end loop;

      J := J * Loop_Par;                          --   12

      for Loop_Par in 5 .. 5 loop
         J := J * Loop_Par;                     --   60
      end loop;

      J := J * Loop_Par;                          --  120

      for Loop_Par in 7 .. 7 loop
         J := J * Loop_Par;                     --  840
      end loop;

      J := J * Loop_Par;                          -- 1680

   end;

   if J /= 1_680 then
      Failed
        ("DID NOT ACCESS IDENTICALLY NAMED LOOP PARAMETER " &
         "INSIDE NON-NESTED LOOPS OR IDENTICALLY NAMED " &
         "VARIABLE OUTSIDE LOOPS");
   end if;

   Result;

end C83b02b;
