-- C54A42G.ADA

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
-- CHECK THAT A  CASE_STATEMENT CORRECTLY HANDLES SEVERAL NON-CONTIGUOUS
--    RANGES OF INTEGERS COVERED BY A SINGLE  'OTHERS'  ALTERNATIVE.

-- (OPTIMIZATION TEST.)

-- RM 03/30/81

with Report;
procedure C54a42g is

   use Report;

begin

   Test
     ("C54A42G",
      "TEST THAT A  CASE_STATEMENT CORRECTLY HANDLES" &
      " SEVERAL NON-CONTIGUOUS RANGES OF INTEGERS" &
      " COVERED BY A SINGLE  'OTHERS'  ALTERNATIVE");

   declare

      Number  : constant         := 2_000;
      Litexpr : constant         := Number + 2_000;
      Statcon : constant Integer := 2_002;
      Dynvar  : Integer          := Ident_Int (0);
      Dyncon  : constant Integer := Ident_Int (1);

   begin

      case Integer'(-4_000) is
         when 100 .. 1_999 =>
            Failed ("WRONG ALTERNATIVE F1");
         when Integer'First .. 0 =>
            null;
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE F3");
         when 2_100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE F4");
         when others =>
            Failed ("WRONG ALTERNATIVE F5");
      end case;

      case Ident_Int (Number) is
         when 100 .. 1_999 =>
            Failed ("WRONG ALTERNATIVE G1");
         when Integer'First .. 0 =>
            Failed ("WRONG ALTERNATIVE G2");
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE G3");
         when 2_100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE G4");
         when others =>
            null;
      end case;

      case Ident_Int (Litexpr) is
         when 100 .. 1_999 =>
            Failed ("WRONG ALTERNATIVE H1");
         when Integer'First .. 0 =>
            Failed ("WRONG ALTERNATIVE H2");
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE H3");
         when 2_100 .. Integer'Last =>
            null;
         when others =>
            Failed ("WRONG ALTERNATIVE H5");
      end case;

      case Ident_Int (Statcon) is
         when 100 .. 1_999 =>
            Failed ("WRONG ALTERNATIVE I1");
         when Integer'First .. 0 =>
            Failed ("WRONG ALTERNATIVE I2");
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE I3");
         when 2_100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE I4");
         when others =>
            null;
      end case;

      case Dynvar is
         when 100 .. 1_999 =>
            Failed ("WRONG ALTERNATIVE J1");
         when Integer'First .. 0 =>
            null;
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE J3");
         when 2_100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE J4");
         when others =>
            Failed ("WRONG ALTERNATIVE J5");
      end case;

      case Dyncon is
         when 100 .. 1_999 =>
            Failed ("WRONG ALTERNATIVE K1");
         when Integer'First .. 0 =>
            Failed ("WRONG ALTERNATIVE K2");
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE K3");
         when 2_100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE K4");
         when others =>
            null;
      end case;

      case Ident_Int (-3_900) is
         when -3_000 .. 1_999 =>
            Failed ("WRONG ALTERNATIVE X1");
         when Integer'First .. -4_000 =>
            Failed ("WRONG ALTERNATIVE X2");
         when 2_001 =>
            Failed ("WRONG ALTERNATIVE X3");
         when 2_100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE X4");
         when others =>
            null;
      end case;

   end;

   Result;

end C54a42g;
