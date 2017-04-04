-- C54A42E.ADA

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
-- CHECK THAT A CASE_STATEMENT CORRECTLY HANDLES A SMALL RANGE OF
--    POTENTIAL VALUES OF TYPE INTEGER, SITUATED FAR FROM  0  AND
--    GROUPED INTO A SMALL NUMBER OF ALTERNATIVES.

-- (OPTIMIZATION TEST -- BIASED JUMP TABLE.)

-- RM 03/26/81

with Report;
procedure C54a42e is

   use Report;

begin

   Test
     ("C54A42E",
      "TEST THAT A  CASE_STATEMENT HANDLES CORRECTLY" &
      " A SMALL, FAR RANGE OF POTENTIAL VALUES OF" &
      " TYPE INTEGER");

   declare

      Number  : constant                              := 4_001;
      Litexpr : constant                              := Number + 5;
      Statcon : constant Integer range 4_000 .. 4_010 := 4_009;
      Dynvar  : Integer range 4_000 .. 4_010          := Ident_Int (4_010);
      Dyncon  : constant Integer range 4_000 .. 4_010 := Ident_Int (4_002);

   begin

      case Integer'(4_000) is
         when 4_001 | 4_004 =>
            Failed ("WRONG ALTERNATIVE F1");
         when 4_009 | 4_002 =>
            Failed ("WRONG ALTERNATIVE F2");
         when 4_005 =>
            Failed ("WRONG ALTERNATIVE F3");
         when 4_003 | 4_007 .. 4_008 =>
            Failed ("WRONG ALTERNATIVE F4");
         when 4_006 =>
            Failed ("WRONG ALTERNATIVE F5");
         when others =>
            null;
      end case;

      case Ident_Int (Number) is
         when 4_001 | 4_004 =>
            null;
         when 4_009 | 4_002 =>
            Failed ("WRONG ALTERNATIVE G2");
         when 4_005 =>
            Failed ("WRONG ALTERNATIVE G3");
         when 4_003 | 4_007 .. 4_008 =>
            Failed ("WRONG ALTERNATIVE G4");
         when 4_006 =>
            Failed ("WRONG ALTERNATIVE G5");
         when others =>
            Failed ("WRONG ALTERNATIVE G6");
      end case;

      case Ident_Int (Litexpr) is
         when 4_001 | 4_004 =>
            Failed ("WRONG ALTERNATIVE H1");
         when 4_009 | 4_002 =>
            Failed ("WRONG ALTERNATIVE H2");
         when 4_005 =>
            Failed ("WRONG ALTERNATIVE H3");
         when 4_003 | 4_007 .. 4_008 =>
            Failed ("WRONG ALTERNATIVE H4");
         when 4_006 =>
            null;
         when others =>
            Failed ("WRONG ALTERNATIVE H6");
      end case;

      case Statcon is
         when 4_001 | 4_004 =>
            Failed ("WRONG ALTERNATIVE I1");
         when 4_009 | 4_002 =>
            null;
         when 4_005 =>
            Failed ("WRONG ALTERNATIVE I3");
         when 4_003 | 4_007 .. 4_008 =>
            Failed ("WRONG ALTERNATIVE I4");
         when 4_006 =>
            Failed ("WRONG ALTERNATIVE I5");
         when others =>
            Failed ("WRONG ALTERNATIVE I6");
      end case;

      case Dynvar is
         when 4_001 | 4_004 =>
            Failed ("WRONG ALTERNATIVE J1");
         when 4_009 | 4_002 =>
            Failed ("WRONG ALTERNATIVE J2");
         when 4_005 =>
            Failed ("WRONG ALTERNATIVE J3");
         when 4_003 | 4_007 .. 4_008 =>
            Failed ("WRONG ALTERNATIVE J4");
         when 4_006 =>
            Failed ("WRONG ALTERNATIVE J5");
         when others =>
            null;

      end case;

      case Dyncon is
         when 4_001 | 4_004 =>
            Failed ("WRONG ALTERNATIVE K1");
         when 4_009 | 4_002 =>
            null;
         when 4_005 =>
            Failed ("WRONG ALTERNATIVE K3");
         when 4_003 | 4_007 .. 4_008 =>
            Failed ("WRONG ALTERNATIVE K4");
         when 4_006 =>
            Failed ("WRONG ALTERNATIVE K5");
         when others =>
            Failed ("WRONG ALTERNATIVE K6");
      end case;

   end;

   Result;

end C54a42e;
