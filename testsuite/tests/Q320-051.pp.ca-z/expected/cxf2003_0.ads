-- CXF2003.A
--
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
--
-- OBJECTIVE:
--      Check that the multiplying operators for a decimal fixed point type
--      return values that are integral multiples of the small of the type.
--      Check the case where the two operands are of different decimal
--      fixed point types.
--
--      Check that if the mathematical result is between multiples of the
--      small of the result type, the result is truncated toward zero.
--      Check that if the attribute 'Round is applied to the mathematical
--      result, however, the result is rounded to the nearest multiple of
--      the small (away from zero if the result is midway between two
--      multiples of the small).
--
-- TEST DESCRIPTION:
--      Two decimal fixed point types A and B are declared, one with a
--      Machine_Radix value of 2, and one with a value of 10. A third decimal
--      fixed point type C is declared with digits and delta values different
--      from those of A and B. For type A (and B), checks are performed
--      on the following operations, where one operand type is C, and the
--      other operand type and the result type is A (or B):
--
--         - Multiplication.
--         - Multiplication, where the attribute 'Round is applied to the
--           result.
--         - Division.
--         - Division, where the attribute 'Round is applied to the result.
--
--      Each operation is performed within a loop, where one operand is
--      always the same variable. After the loop completes, the cumulative
--      total contained in this variable is compared with the expected
--      result.
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for a compiler attempting validation
--      for the Information Systems Annex.
--
--
-- CHANGE HISTORY:
--      22 Mar 96   SAIC    Prerelease version for ACVC 2.1.
--
--!

generic
   type Decimal_Fixed_1 is delta <> digits <>;
   type Decimal_Fixed_2 is delta <> digits <>;
package Cxf2003_0 is

   procedure Multiply_And_Truncate
     (Balance : in out Decimal_Fixed_1; Factor : in Decimal_Fixed_2);

   procedure Divide_And_Truncate
     (Balance : in out Decimal_Fixed_1; Divisor : in Decimal_Fixed_2);

   procedure Multiply_And_Round
     (Balance : in out Decimal_Fixed_1; Factor : in Decimal_Fixed_2);

   procedure Divide_And_Round
     (Balance : in out Decimal_Fixed_1; Divisor : in Decimal_Fixed_2);

end Cxf2003_0;