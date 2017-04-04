-- C45202B.ADA

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
-- CHECK MEMBERSHIP OPERATIONS IN THE CASE IN WHICH A USER HAS
-- REDEFINED THE ORDERING OPERATORS.

-- RJW 1/22/86

with Report; use Report;

procedure C45202b is

begin

   Test
     ("C45202B",
      "CHECK MEMBERSHIP OPERATIONS IN WHICH A USER " &
      "HAS REDEFINED THE ORDERING OPERATORS");

   declare

      type T is (Aa, Bb, Cc, Lit, Xx, Yy, Zz);
      subtype St is T range Aa .. Lit;

      Var : T          := Lit;
      Con : constant T := Lit;

      function ">" (L, R : T) return Boolean is
      begin
         return T'Pos (L) <= T'Pos (R);
      end ">";

      function ">=" (L, R : T) return Boolean is
      begin
         return T'Pos (L) < T'Pos (R);
      end ">=";

      function "<" (L, R : T) return Boolean is
      begin
         return T'Pos (L) >= T'Pos (R);
      end "<";

      function "<=" (L, R : T) return Boolean is
      begin
         return T'Pos (L) > T'Pos (R);
      end "<=";

   begin

      if Lit not in St or
        Var not in St or
        Con not in St or
        not (Var in St) or
        Xx in St or
        not (Xx not in St)
      then
         Failed ("WRONG VALUES FOR 'IN ST'");
      end if;

      if Lit in Aa .. Cc or
        Var not in Lit .. Zz or
        Con in Zz .. Aa or
        not (Cc in Cc .. Yy) or
        not (Bb not in Cc .. Yy)
      then
         Failed ("WRONG VALUES FOR 'IN AA..CC'");
      end if;

   end;

   Result;

end C45202b;
