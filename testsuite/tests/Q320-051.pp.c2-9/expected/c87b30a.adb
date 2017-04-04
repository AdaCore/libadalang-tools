-- C87B30A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- THE EXPRESSION OF A COMPONENT ASSOCIATION MUST MATCH THE TYPE OF THE
-- ASSOCIATED RECORD COMPONENT.

-- TRH  9 AUG 82
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C87b30a is

   type Rec is record
      W, X : Float;
      Y, Z : Integer;
   end record;

   type Flag is (Pass, Fail);

   generic
      type T is private;
      Arg : in T;
      Stat : in Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed
           ("COMPONENT ASSOCIATION EXPRESSION MUST MATCH " &
            "RECORD COMPONENT TYPE");
      end if;
      return Arg;
   end F1;

   function F is new F1 (Float, 2.0, Pass);
   function F is new F1 (Integer, 5, Fail);
   function F is new F1 (Boolean, True, Fail);
   function F is new F1 (Character, 'E', Fail);

   function G is new F1 (Float, 2.0, Fail);
   function G is new F1 (Integer, 5, Pass);
   function G is new F1 (Boolean, True, Fail);
   function G is new F1 (Character, 'E', Fail);

begin
   Test
     ("C87B30A",
      "OVERLOADED EXPRESSIONS IN RECORD AGGREGATE " &
      "COMPONENT ASSOCIATIONS");

   declare
      R1 : Rec := (F, F, G, G);
      R2 : Rec := (X => F, Y => G, Z => G, W => F);
      R3 : Rec := (F, F, Z => G, Y => G);

   begin
      null;
   end;

   Result;
end C87b30a;
