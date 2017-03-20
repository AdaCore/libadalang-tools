-- C67005C.ADA

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
-- CHECK THAT A DECLARATION OF "=" NEED NOT HAVE PARAMETERS
-- OF A LIMITED TYPE IN A RENAMING DECLARATION. THIS TEST CHECKS
-- ACCESS TYPES.

-- BRYCE BARDIN (HUGHES AIRCRAFT) 7/2/84
-- CPP 7/12/84

with Report; use Report;
procedure C67005c is

   generic
      type T is limited private;
      with function Equal (Left, Right : T) return Boolean is <>;
   package Equality is
      function "=" (Left, Right : T) return Boolean;
      -- PRAGMA INLINE ("=");
   end Equality;

   package body Equality is
      function "=" (Left, Right : T) return Boolean is
      begin
         return Equal (Left, Right);
      end "=";
   end Equality;

   package Starter is
      type Int is private;
      function Value_Of (I : Integer) return Int;
      function Equal (Left, Right : Int) return Boolean;
   private
      type Int is access Integer;
   end Starter;

   package body Starter is
      function Value_Of (I : Integer) return Int is
      begin
         return new Integer'(I);
      end Value_Of;

      function Equal (Left, Right : Int) return Boolean is
      begin
         return Left.all = Right.all;
      end Equal;
   end Starter;

   package Abstraction is
      type Int is new Starter.Int;
      package Int_Equality is new Equality (Int, Equal);
      function "=" (Left, Right : Int) return Boolean renames Int_Equality."=";
   end Abstraction;
   use Abstraction;

begin

   Test
     ("C67005C",
      "RENAMING OF EQUALITY OPERATOR WITH " & "NON-LIMITED PARAMETERS");

   declare

      I : Int := Value_Of (1);
      J : Int := Value_Of (0);

      procedure Check (B : Boolean) is
      begin
         if I = J and B then
            Comment ("I = J");
         elsif I /= J and not B then
            Comment ("I /= J");
         else
            Failed ("WRONG ""="" OPERATOR");
         end if;
      end Check;

   begin

      Check (False);
      I := Value_Of (0);
      Check (True);

      Result;

   end;

end C67005c;
