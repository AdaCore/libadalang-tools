-- C54A42C.ADA

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
-- CHECK THAT A CASE_STATEMENT CORRECTLY HANDLES A SPARSE SET OF
--    POTENTIAL VALUES (OF TYPE INTEGER) IN A LARGE RANGE.

--    (OPTIMIZATION TEST)

-- RM 03/26/81

with Report;
procedure C54a42c is

   use Report;

begin

   Test
     ("C54A42C",
      "TEST THAT A  CASE_STATEMENT HANDLES CORRECTLY" &
      " A SPARSE SET OF POTENTIAL VALUES IN A LARGE" & " RANGE");

   declare

      Number  : constant                                 := 1_001;
      Litexpr : constant                                 := Number + 998;
      Statcon : constant Integer range 1 .. Integer'Last := 1_000;
      Dynvar  : Integer range 1 .. Integer'Last          :=
        Ident_Int (Integer'Last - 50);
      Dyncon : constant Integer range 1 .. Integer'Last := Ident_Int (1_000);

   begin

      case Integer'(Number) is
         when 1 .. 10 =>
            Failed ("WRONG ALTERNATIVE F1");
         when 1_000 =>
            Failed ("WRONG ALTERNATIVE F2");
         when 2_000 =>
            Failed ("WRONG ALTERNATIVE F3");
         when 4_000 .. 4_100 =>
            Failed ("WRONG ALTERNATIVE F4");
         when Integer'Last - 100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE F5");
         when others =>
            null;
      end case;

      case Ident_Int (10) is
         when 1 .. 10 =>
            null;
         when 1_000 =>
            Failed ("WRONG ALTERNATIVE G2");
         when 2_000 =>
            Failed ("WRONG ALTERNATIVE G3");
         when 4_000 .. 4_100 =>
            Failed ("WRONG ALTERNATIVE G4");
         when Integer'Last - 100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE G5");
         when others =>
            Failed ("WRONG ALTERNATIVE G6");
      end case;

      case Ident_Int (Litexpr) is
         when 1 .. 10 =>
            Failed ("WRONG ALTERNATIVE H1");
         when 1_000 =>
            Failed ("WRONG ALTERNATIVE H2");
         when 2_000 =>
            Failed ("WRONG ALTERNATIVE H3");
         when 4_000 .. 4_100 =>
            Failed ("WRONG ALTERNATIVE H4");
         when Integer'Last - 100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE H5");
         when others =>
            null;
      end case;

      case Statcon is
         when 1 .. 10 =>
            Failed ("WRONG ALTERNATIVE I1");
         when 1_000 =>
            null;
         when 2_000 =>
            Failed ("WRONG ALTERNATIVE I3");
         when 4_000 .. 4_100 =>
            Failed ("WRONG ALTERNATIVE I4");
         when Integer'Last - 100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE I5");
         when others =>
            Failed ("WRONG ALTERNATIVE I6");
      end case;

      case Dynvar is
         when 1 .. 10 =>
            Failed ("WRONG ALTERNATIVE J1");
         when 1_000 =>
            Failed ("WRONG ALTERNATIVE J2");
         when 2_000 =>
            Failed ("WRONG ALTERNATIVE J3");
         when 4_000 .. 4_100 =>
            Failed ("WRONG ALTERNATIVE J4");
         when Integer'Last - 100 .. Integer'Last =>
            null;
         when others =>
            Failed ("WRONG ALTERNATIVE J6");
      end case;

      case Dyncon is
         when 1 .. 10 =>
            Failed ("WRONG ALTERNATIVE K1");
         when 1_000 =>
            null;
         when 2_000 =>
            Failed ("WRONG ALTERNATIVE K3");
         when 4_000 .. 4_100 =>
            Failed ("WRONG ALTERNATIVE K4");
         when Integer'Last - 100 .. Integer'Last =>
            Failed ("WRONG ALTERNATIVE K5");
         when others =>
            Failed ("WRONG ALTERNATIVE K6");
      end case;

   end;

   Result;

end C54a42c;
