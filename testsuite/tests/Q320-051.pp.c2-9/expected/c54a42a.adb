-- C54A42A.ADA

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
-- CHECK THAT A CASE_STATEMENT MAY HANDLE A LARGE NUMBER OF
--    POTENTIAL VALUES GROUPED INTO A SMALL NUMBER OF ALTERNATIVES
--    AND THAT EACH TIME THE APPROPRIATE ALTERNATIVE IS EXECUTED.

-- (OPTIMIZATION TEST.)

-- RM 03/24/81
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.

with Report;
procedure C54a42a is

   use Report;

begin

   Test
     ("C54A42A",
      "TEST THAT A  CASE_STATEMENT HANDLES CORRECTLY" &
      " A LARGE NUMBER OF POTENTIAL VALUES GROUPED" &
      " INTO A SMALL NUMBER OF ALTERNATIVES");

   declare

      Statcon : constant Character := 'B';
      Statvar : Character          := 'Q';
      Dyncon  : constant Character := Ident_Char ('Y');
      Dynvar  : Character          := Ident_Char ('Z');

   begin

      case Character'('A') is
         when Ascii.Nul .. 'A' =>
            null;
         when 'B' =>
            Failed ("WRONG ALTERN. A2");
         when 'P' =>
            Failed ("WRONG ALTERN. A3");
         when 'Y' =>
            Failed ("WRONG ALTERN. A4");
         when 'Z' .. Ascii.Del =>
            Failed ("WRONG ALTERN. A5");
         when others =>
            Failed ("WRONG ALTERN. A6");
      end case;

      case Statcon is
         when Ascii.Nul .. 'A' =>
            Failed ("WRONG ALTERN. B1");
         when 'B' =>
            null;
         when 'P' =>
            Failed ("WRONG ALTERN. B3");
         when 'Y' =>
            Failed ("WRONG ALTERN. B4");
         when 'Z' .. Ascii.Del =>
            Failed ("WRONG ALTERN. B5");
         when others =>
            Failed ("WRONG ALTERN. B6");
      end case;

      case Statvar is
         when Ascii.Nul .. 'A' =>
            Failed ("WRONG ALTERN. C1");
         when 'B' =>
            Failed ("WRONG ALTERN. C2");
         when 'P' =>
            Failed ("WRONG ALTERN. C3");
         when 'Y' =>
            Failed ("WRONG ALTERN. C4");
         when 'Z' .. Ascii.Del =>
            Failed ("WRONG ALTERN. C5");
         when others =>
            null;
      end case;

      case Dyncon is
         when Ascii.Nul .. 'A' =>
            Failed ("WRONG ALTERN. D1");
         when 'B' =>
            Failed ("WRONG ALTERN. D2");
         when 'P' =>
            Failed ("WRONG ALTERN. D3");
         when 'Y' =>
            null;
         when 'Z' .. Ascii.Del =>
            Failed ("WRONG ALTERN. D5");
         when others =>
            Failed ("WRONG ALTERN. D6");
      end case;

      case Dynvar is
         when Ascii.Nul .. 'A' =>
            Failed ("WRONG ALTERN. E1");
         when 'B' =>
            Failed ("WRONG ALTERN. E2");
         when 'P' =>
            Failed ("WRONG ALTERN. E3");
         when 'Y' =>
            Failed ("WRONG ALTERN. E4");
         when 'Z' .. Ascii.Del =>
            null;
         when others =>
            Failed ("WRONG ALTERN. E6");
      end case;

   end;

   declare

      Number  : constant         := -100;
      Litexpr : constant         := 0 * Number + 16;
      Statcon : constant Integer := +100;
      Dynvar  : Integer          := Ident_Int (102);
      Dyncon  : constant Integer := Ident_Int (17);

   begin

      case Integer'(-102) is
         when Integer'First .. -101 =>
            null;
         when -100 =>
            Failed ("WRONG ALTERN. F2");
         when 17 =>
            Failed ("WRONG ALTERN. F2");
         when 100 =>
            Failed ("WRONG ALTERN. F4");
         when 101 .. Integer'Last =>
            Failed ("WRONG ALTERN. F5");
         when others =>
            Failed ("WRONG ALTERN. F6");
      end case;

      case Ident_Int (Number) is
         when Integer'First .. -101 =>
            Failed ("WRONG ALTERN. G1");
         when -100 =>
            null;
         when 17 =>
            Failed ("WRONG ALTERN. G3");
         when 100 =>
            Failed ("WRONG ALTERN. G4");
         when 101 .. Integer'Last =>
            Failed ("WRONG ALTERN. G5");
         when others =>
            Failed ("WRONG ALTERN. G6");
      end case;

      case Ident_Int (Litexpr) is
         when Integer'First .. -101 =>
            Failed ("WRONG ALTERN. H1");
         when -100 =>
            Failed ("WRONG ALTERN. H2");
         when 17 =>
            Failed ("WRONG ALTERN. H3");
         when 100 =>
            Failed ("WRONG ALTERN. H4");
         when 101 .. Integer'Last =>
            Failed ("WRONG ALTERN. H5");
         when others =>
            null;
      end case;

      case Statcon is
         when Integer'First .. -101 =>
            Failed ("WRONG ALTERN. I1");
         when -100 =>
            Failed ("WRONG ALTERN. I2");
         when 17 =>
            Failed ("WRONG ALTERN. I3");
         when 100 =>
            null;
         when 101 .. Integer'Last =>
            Failed ("WRONG ALTERN. I5");
         when others =>
            Failed ("WRONG ALTERN. I6");
      end case;

      case Dynvar is
         when Integer'First .. -101 =>
            Failed ("WRONG ALTERN. J1");
         when -100 =>
            Failed ("WRONG ALTERN. J2");
         when 17 =>
            Failed ("WRONG ALTERN. J3");
         when 100 =>
            Failed ("WRONG ALTERN. J4");
         when 101 .. Integer'Last =>
            null;
         when others =>
            Failed ("WRONG ALTERN. J6");
      end case;

      case Dyncon is
         when Integer'First .. -101 =>
            Failed ("WRONG ALTERN. K1");
         when -100 =>
            Failed ("WRONG ALTERN. K2");
         when 17 =>
            null;
         when 100 =>
            Failed ("WRONG ALTERN. K4");
         when 101 .. Integer'Last =>
            Failed ("WRONG ALTERN. K5");
         when others =>
            Failed ("WRONG ALTERN. K6");
      end case;
   end;

   Result;

end C54a42a;
