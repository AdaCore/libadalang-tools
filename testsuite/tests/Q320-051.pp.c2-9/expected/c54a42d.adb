-- C54A42D.ADA

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
-- CHECK THAT A CASE_STATEMENT CORRECTLY HANDLES A FEW ALTERNATIVES
--    COVERING A LARGE RANGE OF INTEGERS.

-- (OPTIMIZATION TEST.)

-- RM 03/30/81

with Report;
procedure C54a42d is

   use Report;

begin

   Test
     ("C54A42D",
      "TEST THAT A  CASE_STATEMENT CORRECTLY HANDLES" &
      " A FEW ALTERNATIVES COVERING A LARGE RANGE" & " OF INTEGERS");

   declare

      Number  : constant         := 2_000;
      Litexpr : constant         := Number + 2_000;
      Statcon : constant Integer := 2_001;
      Dynvar  : Integer          := Ident_Int (0);
      Dyncon  : constant Integer := Ident_Int (1);

   begin

      case Integer'(-4_000) is
         when 1 .. 2_000 =>
            Failed ("WRONG ALTERNATIVE F1");
         when Integer'First .. 0 =>
            null;
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE F3");
         when 2_002 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE F4");
      end case;

      case Integer'(Number) is
         when 1 .. 2_000 =>
            null;
         when Integer'First .. 0 =>
            Failed ("WRONG ALTERNATIVE G2");
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE G3");
         when 2_002 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE G4");
      end case;

      case Ident_Int (Litexpr) is
         when 1 .. 2_000 =>
            Failed ("WRONG ALTERNATIVE H1");
         when Integer'First .. 0 =>
            Failed ("WRONG ALTERNATIVE H2");
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE H3");
         when 2_002 .. Integer'Last =>
            null;
      end case;

      case Statcon is
         when 1 .. 2_000 =>
            Failed ("WRONG ALTERNATIVE I1");
         when Integer'First .. 0 =>
            Failed ("WRONG ALTERNATIVE I2");
         when 2_001 =>
            null;
         when 2_002 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE I4");
      end case;

      case Dynvar is
         when 1 .. 2_000 =>
            Failed ("WRONG ALTERNATIVE J1");
         when Integer'First .. 0 =>
            null;
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE J3");
         when 2_002 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE J4");
      end case;

      case Dyncon is
         when 1 .. 2_000 =>
            null;
         when Integer'First .. 0 =>
            Failed ("WRONG ALTERNATIVE K2");
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE K3");
         when 2_002 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE K4");
      end case;

   end;

   Result;

end C54a42d;
