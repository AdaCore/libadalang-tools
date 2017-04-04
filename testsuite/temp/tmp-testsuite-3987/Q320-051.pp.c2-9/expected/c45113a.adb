-- C45113A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE OPERANDS OF LOGICAL
-- OPERATORS HAVE DIFFERENT LENGTHS.

-- RJW  1/15/86

with Report; use Report;

procedure C45113a is

begin

   Test
     ("C45113A",
      "CHECK ON LOGICAL OPERATORS WITH " & "OPERANDS OF DIFFERENT LENGTHS");

   declare

      type Arr is array (Integer range <>) of Boolean;

      A : Arr (Ident_Int (1) .. Ident_Int (2)) := (True, False);
      B : Arr (Ident_Int (1) .. Ident_Int (3)) := (True, False, True);

   begin

      begin -- TEST FOR 'AND'.
         if (A and B) = B then
            Failed ("A AND B = B");
         end if;
         Failed ("NO EXCEPTION RAISED FOR 'AND'");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR 'AND'");
      end;

      begin -- TEST FOR 'OR'.
         if (A or B) = B then
            Failed ("A OR B = B");
         end if;
         Failed ("NO EXCEPTION RAISED FOR 'OR'");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR 'OR'");
      end;

      begin -- TEST FOR 'XOR'.
         if (A xor B) = B then
            Failed ("A XOR B = B");
         end if;
         Failed ("NO EXCEPTION RAISED FOR 'XOR'");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR 'XOR'");
      end;

   end;

   Result;

end C45113a;
