-- C87B14D.ADA

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
-- IN SUBTYPE INDICATIONS WITH INDEX CONSTRAINTS, IF A BOUND IS OF TYPE
-- UNIVERSAL_INTEGER, IT IS IMPLICITLY CONVERTED TO THE INDEX BASE TYPE.

-- TRH  7 JULY 82

with Report; use Report;

procedure C87b14d is

   type Whole is new Integer range 0 .. Integer'Last;
   type List is array (Whole range <>) of Boolean;

begin
   Test
     ("C87B14D",
      "OVERLOADED EXPRESSIONS IN INDEX CONSTRAINTS " &
      "OF SUBTYPE INDICATIONS WITH UNIVERSAL_INTEGER BOUNDS");

   declare
      function "+" (X, Y : Whole) return Whole renames "*";

      subtype List1 is List (1 + 1 .. 1 + 1);
      subtype List2 is List (1 .. 3 + 3);
      subtype List3 is List (1 + 1 .. 2);

   begin
      if List1'First /= 1 or List1'Last /= 1 or List2'First /= 1 or
        List2'Last /= 9 or List3'First /= 1 or List3'Last /= 2 then
         Failed
           ("RESOLUTION INCORRECT - IMPLICIT CONVERSION " &
            "OF UNIVERSAL_INTEGER TYPE TO INDEX CONSTRAINT " & "BASE TYPE");
      end if;
   end;

   Result;
end C87b14d;
