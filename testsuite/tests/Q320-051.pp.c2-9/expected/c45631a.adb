-- C45631A.ADA

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
-- CHECK THAT FOR TYPE INTEGER 'ABS A' EQUALS A IF A IS POSITIVE AND EQUALS -A
-- IF A IS NEGATIVE.

-- RJW 2/10/86

with Report; use Report;

procedure C45631a is

begin

   Test
     ("C45631A",
      "CHECK THAT FOR TYPE INTEGER 'ABS A' " &
      "EQUALS A IF A IS POSITIVE AND EQUALS -A IF " & "A IS NEGATIVE");

   declare

      P : Integer := Ident_Int (1);
      N : Integer := Ident_Int (-1);
      Z : Integer := Ident_Int (0);
   begin

      if abs P = P then
         null;
      else
         Failed ("'ABS' TEST FOR P - 1");
      end if;

      if abs N = -N then
         null;
      else
         Failed ("'ABS' TEST FOR N - 1");
      end if;

      if abs Z = Z then
         null;
      else
         Failed ("'ABS TEST FOR Z - 1");
      end if;

      if abs (Z) = -Z then
         null;
      else
         Failed ("'ABS TEST FOR Z - 2");
      end if;

      if "ABS" (Right => P) = P then
         null;
      else
         Failed ("'ABS' TEST FOR P - 2");
      end if;

      if "ABS" (N) = -N then
         null;
      else
         Failed ("'ABS' TEST FOR N - 2 ");
      end if;

      if "ABS" (Z) = Z then
         null;
      else
         Failed ("'ABS' TEST FOR Z - 3");
      end if;

      if abs (Ident_Int (-Integer'Last)) = Integer'Last then
         null;
      else
         Failed ("'ABS' TEST FOR -INTEGER'LAST");
      end if;
   end;

   Result;

end C45631a;