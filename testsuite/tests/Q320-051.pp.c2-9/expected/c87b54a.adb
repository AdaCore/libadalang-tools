-- C87B54A.ADA

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
-- THE ARGUMENT OF THE DELAY STATEMENT IS OF THE PREDEFINED FIXED
-- POINT TYPE DURATION.

-- TRH  7 SEPT 82

with Report; use Report;

procedure C87b54a is

   type Temps is new Duration;
   type Real is new Float;
   type Tempus is delta 0.1 range -1.0 .. 1.0;
   Err : Boolean := False;

   function F (X : Temps) return Temps is
   begin
      Err := True;
      return X;
   end F;

   function F (X : Real) return Real is
   begin
      Err := True;
      return X;
   end F;

   function F (X : Tempus) return Tempus is
   begin
      Err := True;
      return X;
   end F;

   function F (X : Duration) return Duration is
   begin
      return X;
   end F;

begin
   Test ("C87B54A", "OVERLOADED EXPRESSION WITHIN DELAY STATEMENT");

   declare
      task T is
         entry E;
      end T;

      task body T is
      begin
         delay F (0.0);
         delay F (1.0);
         delay F (-1.0);
      end T;

   begin
      if Err then
         Failed
           ("DELAY STATEMENT TAKES AN ARGUMENT OF " &
            "THE PREDEFINED FIXED POINT TYPE " &
            "DURATION");
      end if;
   end;

   Result;
end C87b54a;
