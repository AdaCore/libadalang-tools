-- C35A08B.ADA

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
--     CHECK THAT THE MULTIPLICATION AND DIVISION OPERATORS FOR TWO
--     FIXED POINT OPERANDS ARE DECLARED IN STANDARD AND ARE DIRECTLY
--     VISIBLE.

-- HISTORY:
--     BCB 01/21/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C35a08b is

   package P is
      type T1 is delta 2.0**(-4) range -100.0 .. 100.0;
      type T2 is delta 2.0**(-4) range -100.0 .. 100.0;
   end P;
   use P;

   X1 : P.T1 := 6.0;
   X2 : P.T1 := 2.0;
   X3 : P.T1;
   X4 : P.T1;
   X5 : P.T1;
   X6 : P.T1;

   X7 : P.T2 := 2.0;

   function Ident_Fixed (X : P.T1) return P.T1 is
   begin
      return X * Ident_Int (1);
   end Ident_Fixed;

begin
   Test
     ("C35A08B",
      "CHECK THAT THE MULTIPLICATION AND DIVISION " &
      "OPERATORS FOR TWO FIXED POINT OPERANDS ARE " &
      "DECLARED IN STANDARD AND ARE DIRECTLY VISIBLE");

   X3 := P.T1 (X1 * X2);
   X4 := P.T1 (X1 / X2);

   X5 := P.T1 (Standard."*" (X1, X2));
   X6 := P.T1 (Standard."/" (X1, X2));

   if X3 /= Ident_Fixed (12.0) then
      Failed ("IMPROPER VALUE FOR FIXED POINT MULTIPLICATION - 1");
   end if;

   if X4 /= Ident_Fixed (3.0) then
      Failed ("IMPROPER VALUE FOR FIXED POINT DIVISION - 1");
   end if;

   X3 := P.T1 (X1 * X7);
   X4 := P.T1 (X1 / X7);

   X5 := P.T1 (Standard."*" (X1, X7));
   X6 := P.T1 (Standard."/" (X1, X7));

   if X3 /= Ident_Fixed (12.0) then
      Failed ("IMPROPER VALUE FOR FIXED POINT MULTIPLICATION - 2");
   end if;

   if X4 /= Ident_Fixed (3.0) then
      Failed ("IMPROPER VALUE FOR FIXED POINT DIVISION - 2");
   end if;

   Result;
end C35a08b;
