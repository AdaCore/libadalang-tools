-- C87B43A.ADA

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
-- IN A CASE STATEMENT, THE TYPE OF EACH CHOICE MUST MATCH THE TYPE
-- OF THE EXPRESSION.

-- TRH  3 AUG 82
-- DSJ 10 JUN 83

with Report; use Report;

procedure C87b43a is

   type Whole is new Integer range 0 .. Integer'Last;

   function "+" (X, Y : Integer) return Integer renames "*";

   Err : Boolean := False;
   X   : Whole   := 6;

begin
   Test ("C87B43A", "TYPE OF CASE CHOICE MUST MATCH TYPE OF " & "EXPRESSION");

   case X is
      when (2 + 3) =>
         Err := True;
      when (3 + 3) =>
         null;
      when others =>
         Err := True;
   end case;

   if Err then
      Failed ("CASE STATEMENT CHOICE MUST MATCH TYPE OF EXPRESSION");
   end if;

   Result;
end C87b43a;
