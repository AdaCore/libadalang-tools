-- C48005B.ADA

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
-- CHECK THAT AN ALLOCATOR OF THE FORM "NEW T X" ALLOCATES A NEW OBJECT
-- EACH TIME IT IS EXECUTED AND THAT IF X IS AN INDEX CONSTRAINT AND T
-- AN UNCONSTRAINED ARRAY TYPE, THE ALLOCATED OBJECT HAS THE INDEX
-- BOUNDS SPECIFIED BY X.

-- EG  08/10/84

with Report;

procedure C48005b is

   use Report;

begin

   Test
     ("C48005B",
      "CHECK THAT THE FORM 'NEW T X' ALLOCATES A " &
      "NEW OBJECT AND THAT IF X IS AN INDEX " &
      "CONSTRAINT AND T AN UNCONSTRAINED ARRAY " &
      "TYPE, THE ALLOCATED OBJECT HAS THE INDEX " &
      "BOUND SPECIFIED BY X");

   declare

      type Ua1 is array (Integer range <>) of Integer;
      type Ua2 is array (Integer range <>, Integer range <>) of Integer;

      type A_Ua1 is access Ua1;
      type A_Ua2 is access Ua2;

      V_A_Ua1 : A_Ua1;
      V_A_Ua2 : A_Ua2;

   begin

      V_A_Ua1 := new Ua1 (4 .. 7);
      if (V_A_Ua1'First /= Ident_Int (4) or V_A_Ua1'Last /= Ident_Int (7)) then
         Failed ("WRONG ARRAY BOUNDS - V_A_UA1");
      end if;

      V_A_Ua2 := new Ua2 (2 .. 3, 4 .. 6);
      if
        (V_A_Ua2'First (1) /= Ident_Int (2) or
         V_A_Ua2'Last (1) /= Ident_Int (3) or
         V_A_Ua2'First (2) /= Ident_Int (4) or
         V_A_Ua2'Last (2) /= Ident_Int (6))
      then
         Failed ("WRONG ARRAY BOUNDS - V_A_UA2");
      end if;

   end;

   Result;

end C48005b;
