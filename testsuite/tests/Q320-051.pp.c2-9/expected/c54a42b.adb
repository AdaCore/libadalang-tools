-- C54A42B.ADA

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
--    POTENTIAL VALUES GROUPED INTO A SMALL NUMBER OF ALTERNATIVES.

-- (OPTIMIZATION TEST -- JUMP TABLE.)

-- RM 03/26/81
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.

with Report;
procedure C54a42b is

   use Report;

begin

   Test
     ("C54A42B",
      "TEST THAT A  CASE_STATEMENT HANDLES CORRECTLY" &
      " A SMALL NUMBER OF POTENTIAL VALUES GROUPED" &
      " INTO A SMALL NUMBER OF ALTERNATIVES");

   declare

      Statcon : constant Character range 'A' .. 'K' := 'J';
      Statvar : Character range 'A' .. 'K'          := 'A';
      Dyncon  : constant Character range 'A' .. 'K' := Ident_Char ('K');
      Dynvar  : Character range 'A' .. 'K'          := Ident_Char ('G');

   begin

      case Statvar is
         when 'B' | 'E' =>
            Failed ("WRONG ALTERNATIVE A1");
         when 'J' | 'C' =>
            Failed ("WRONG ALTERNATIVE A2");
         when 'F' =>
            Failed ("WRONG ALTERNATIVE A3");
         when 'D' | 'H' .. 'I' =>
            Failed ("WRONG ALTERNATIVE A4");
         when 'G' =>
            Failed ("WRONG ALTERNATIVE A5");
         when others =>
            null;
      end case;

      case Character'('B') is
         when 'B' | 'E' =>
            null;
         when 'J' | 'C' =>
            Failed ("WRONG ALTERNATIVE B2");
         when 'F' =>
            Failed ("WRONG ALTERNATIVE B3");
         when 'D' | 'H' .. 'I' =>
            Failed ("WRONG ALTERNATIVE B4");
         when 'G' =>
            Failed ("WRONG ALTERNATIVE B5");
         when others =>
            Failed ("WRONG ALTERNATIVE B6");
      end case;

      case Dynvar is
         when 'B' | 'E' =>
            Failed ("WRONG ALTERNATIVE C1");
         when 'J' | 'C' =>
            Failed ("WRONG ALTERNATIVE C2");
         when 'F' =>
            Failed ("WRONG ALTERNATIVE C3");
         when 'D' | 'H' .. 'I' =>
            Failed ("WRONG ALTERNATIVE C4");
         when 'G' =>
            null;
         when others =>
            Failed ("WRONG ALTERNATIVE C6");
      end case;

      case Ident_Char (Statcon) is
         when 'B' | 'E' =>
            Failed ("WRONG ALTERNATIVE D1");
         when 'J' | 'C' =>
            null;
         when 'F' =>
            Failed ("WRONG ALTERNATIVE D3");
         when 'D' | 'H' .. 'I' =>
            Failed ("WRONG ALTERNATIVE D4");
         when 'G' =>
            Failed ("WRONG ALTERNATIVE D5");
         when others =>
            Failed ("WRONG ALTERNATIVE D6");
      end case;

      case Dyncon is
         when 'B' | 'E' =>
            Failed ("WRONG ALTERNATIVE E1");
         when 'J' | 'C' =>
            Failed ("WRONG ALTERNATIVE E2");
         when 'F' =>
            Failed ("WRONG ALTERNATIVE E3");
         when 'D' | 'H' .. 'I' =>
            Failed ("WRONG ALTERNATIVE E4");
         when 'G' =>
            Failed ("WRONG ALTERNATIVE E5");
         when others =>
            null;
      end case;

   end;

   declare

      Number  : constant                       := 1;
      Litexpr : constant                       := Number + 5;
      Statcon : constant Integer range 0 .. 10 := 9;
      Dynvar  : Integer range 0 .. 10          := Ident_Int (10);
      Dyncon  : constant Integer range 0 .. 10 := Ident_Int (2);

   begin

      case Integer'(0) is
         when 1 | 4 =>
            Failed ("WRONG ALTERNATIVE F1");
         when 9 | 2 =>
            Failed ("WRONG ALTERNATIVE F2");
         when 5 =>
            Failed ("WRONG ALTERNATIVE F3");
         when 3 | 7 .. 8 =>
            Failed ("WRONG ALTERNATIVE F4");
         when 6 =>
            Failed ("WRONG ALTERNATIVE F5");
         when others =>
            null;
      end case;

      case Integer'(Number) is
         when 1 | 4 =>
            null;
         when 9 | 2 =>
            Failed ("WRONG ALTERNATIVE G2");
         when 5 =>
            Failed ("WRONG ALTERNATIVE G3");
         when 3 | 7 .. 8 =>
            Failed ("WRONG ALTERNATIVE G4");
         when 6 =>
            Failed ("WRONG ALTERNATIVE G5");
         when others =>
            Failed ("WRONG ALTERNATIVE G6");
      end case;

      case Ident_Int (Litexpr) is
         when 1 | 4 =>
            Failed ("WRONG ALTERNATIVE H1");
         when 9 | 2 =>
            Failed ("WRONG ALTERNATIVE H2");
         when 5 =>
            Failed ("WRONG ALTERNATIVE H3");
         when 3 | 7 .. 8 =>
            Failed ("WRONG ALTERNATIVE H4");
         when 6 =>
            null;
         when others =>
            Failed ("WRONG ALTERNATIVE H6");
      end case;

      case Statcon is
         when 1 | 4 =>
            Failed ("WRONG ALTERNATIVE I1");
         when 9 | 2 =>
            null;
         when 5 =>
            Failed ("WRONG ALTERNATIVE I3");
         when 3 | 7 .. 8 =>
            Failed ("WRONG ALTERNATIVE I4");
         when 6 =>
            Failed ("WRONG ALTERNATIVE I5");
         when others =>
            Failed ("WRONG ALTERNATIVE I6");
      end case;

      case Dynvar is
         when 1 | 4 =>
            Failed ("WRONG ALTERNATIVE J1");
         when 9 | 2 =>
            Failed ("WRONG ALTERNATIVE J2");
         when 5 =>
            Failed ("WRONG ALTERNATIVE J3");
         when 3 | 7 .. 8 =>
            Failed ("WRONG ALTERNATIVE J4");
         when 6 =>
            Failed ("WRONG ALTERNATIVE J5");
         when others =>
            null;
      end case;

      case Dyncon is
         when 1 | 4 =>
            Failed ("WRONG ALTERNATIVE K1");
         when 9 | 2 =>
            null;
         when 5 =>
            Failed ("WRONG ALTERNATIVE K3");
         when 3 | 7 .. 8 =>
            Failed ("WRONG ALTERNATIVE K4");
         when 6 =>
            Failed ("WRONG ALTERNATIVE K5");
         when others =>
            Failed ("WRONG ALTERNATIVE K6");
      end case;

   end;

   Result;

end C54a42b;
