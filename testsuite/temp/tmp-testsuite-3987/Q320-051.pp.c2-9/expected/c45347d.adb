-- C45347D.ADA

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
-- CHECK THAT CATENATION IS DEFINED FOR ACCESS TYPES AS COMPONENT TYPES.

-- JWC 11/15/85

with Report; use Report;

procedure C45347d is

begin

   Test
     ("C45347D",
      "CHECK THAT CATENATION IS DEFINED " &
      "FOR ACCESS TYPES AS COMPONENT TYPES");

   declare

      subtype Int is Integer range 1 .. 4;
      type Acc is access Int;
      type A is array (Int range <>) of Acc;

      Ac1 : Acc := new Int'(1);
      Ac2 : Acc := new Int'(2);
      Ac3 : Acc := new Int'(3);
      Ac4 : Acc := new Int'(4);

      A1 : A (1 .. 2) := (Ac1, Ac2);
      A2 : A (1 .. 2) := (Ac3, Ac4);
      A3 : A (1 .. 4) := (Ac1, Ac2, Ac3, Ac4);
      A4 : A (1 .. 4);
      A5 : A (1 .. 4) := (Ac4, Ac3, Ac2, Ac1);

   begin

      A4 := A1 & A2;

      if A3 /= A4 then
         Failed ("INCORRECT CATENATION FOR TWO ARRAYS OF ACCESS");
      end if;

      A4 := A5;

      A4 := A1 & A2 (1) & Ac4;

      if A3 /= A4 then
         Failed ("INCORRECT CATENATION FOR ARRAY OF ACCESS, " & "AND ACCESS");
      end if;

      A4 := A5;

      A4 := Ac1 & (A1 (2) & A2);

      if A3 /= A4 then
         Failed ("INCORRECT CATENATION FOR ACCESS, AND ARRAY " & "OF ACCESS");
      end if;

      A4 := A5;

      A4 := Ac1 & A1 (2) & (A2 (1) & Ac4);

      if A3 /= A4 then
         Failed ("INCORRECT CATENATION FOR ACCESS");
      end if;

   end;

   Result;

end C45347d;
