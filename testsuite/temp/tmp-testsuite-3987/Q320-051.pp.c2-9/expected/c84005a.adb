-- C84005A.ADA

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
--     CHECK THAT TWO POTENTIALLY VISIBLE HOMOGRAPHS OF A SUBPROGRAM
--     IDENTIFIER CAN BE MADE DIRECTLY VISIBLE BY A USE CLAUSE, AND THAT
--     WHEN DIFFERENT FORMAL PARAMETER NAMES ARE USED THE SUBPROGRAMS
--     ARE REFERENCED CORRECTLY.

-- HISTORY:
--     JET 03/10/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C84005a is

   package Pack1 is
      function Funk (A : Integer) return Integer;
      procedure Prok (A : Integer; B : out Integer);
   end Pack1;

   package Pack2 is
      function Funk (X : Integer) return Integer;
      procedure Prok (X : Integer; Y : out Integer);
   end Pack2;

   use Pack1, Pack2;
   Var1, Var2 : Integer;

   package body Pack1 is
      function Funk (A : Integer) return Integer is
      begin
         if Equal (A, A) then
            return (1);
         else
            return (0);
         end if;
      end Funk;

      procedure Prok (A : Integer; B : out Integer) is
      begin
         if Equal (A, A) then
            B := 1;
         else
            B := 0;
         end if;
      end Prok;
   end Pack1;

   package body Pack2 is
      function Funk (X : Integer) return Integer is
      begin
         if Equal (X, X) then
            return (2);
         else
            return (0);
         end if;
      end Funk;

      procedure Prok (X : Integer; Y : out Integer) is
      begin
         if Equal (X, X) then
            Y := 2;
         else
            Y := 0;
         end if;
      end Prok;
   end Pack2;

begin
   Test
     ("C84005A",
      "CHECK THAT TWO POTENTIALLY VISIBLE HOMOGRAPHS " &
      "OF A SUBPROGRAM IDENTIFIER CAN BE MADE " &
      "DIRECTLY VISIBLE BY A USE CLAUSE, AND THAT " &
      "WHEN DIFFERENT FORMAL PARAMETER NAMES ARE " &
      "USED, THE SUBPROGRAMS ARE REFERENCED CORRECTLY");

   if Funk (A => 3) /= Ident_Int (1) then
      Failed ("PACK1.FUNK RETURNS INCORRECT RESULT");
   end if;

   if Funk (X => 3) /= Ident_Int (2) then
      Failed ("PACK2.FUNK RETURNS INCORRECT RESULT");
   end if;

   Prok (A => 3, B => Var1);
   Prok (X => 3, Y => Var2);

   if Var1 /= Ident_Int (1) then
      Failed ("PACK1.PROK RETURNS INCORRECT RESULT");
   end if;

   if Var2 /= Ident_Int (2) then
      Failed ("PACK2.PROK RETURNS INCORRECT RESULT");
   end if;

   Result;
end C84005a;
