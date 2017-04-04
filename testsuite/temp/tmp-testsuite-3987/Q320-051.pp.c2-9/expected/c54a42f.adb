-- C54A42F.ADA

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
-- CHECK THAT A  CASE_STATEMENT CORRECTLY HANDLES SEVERAL SMALL,
--    NON-CONTIGUOUS RANGES OF INTEGERS COVERED BY A SINGLE  'OTHERS'
--    ALTERNATIVE.

-- (OPTIMIZATION TEST.)

-- RM 03/31/81

with Report;
procedure C54a42f is

   use Report;

begin

   Test
     ("C54A42F",
      "TEST THAT A  CASE_STATEMENT CORRECTLY HANDLES" &
      " SEVERAL SMALL, NON-CONTIGUOUS ENUMERATION" &
      " RANGES COVERED BY A SINGLE  'OTHERS'  " &
      " ALTERNATIVE");

   declare

      type Day is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

      Dynvar2 : Day          := Mon;
      Statvar : Day          := Tue;
      Statcon : constant Day := Wed;
      Dynvar  : Day          := Thu;
      Dyncon  : constant Day := Day'Val (Ident_Int (5)); -- FRI

   begin

      if Equal (1, 289) then
         Dynvar  := Sun;
         Dynvar2 := Sun;
      end if;

      case Sun is        --  SUN
         when Thu =>
            Failed ("WRONG ALTERNATIVE F1");
         when Sun =>
            null;
         when Sat =>
            Failed ("WRONG ALTERNATIVE F3");
         when Tue .. Wed =>
            Failed ("WRONG ALTERNATIVE F4");
         when others =>
            Failed ("WRONG ALTERNATIVE F5");
      end case;

      case Dynvar2 is   --  MON
         when Thu =>
            Failed ("WRONG ALTERNATIVE G1");
         when Sun =>
            Failed ("WRONG ALTERNATIVE G2");
         when Sat =>
            Failed ("WRONG ALTERNATIVE G3");
         when Tue .. Wed =>
            Failed ("WRONG ALTERNATIVE G4");
         when others =>
            null;
      end case;

      case Statvar is    --  TUE
         when Thu =>
            Failed ("WRONG ALTERNATIVE H1");
         when Sun =>
            Failed ("WRONG ALTERNATIVE H2");
         when Sat =>
            Failed ("WRONG ALTERNATIVE H3");
         when Tue .. Wed =>
            null;
         when others =>
            Failed ("WRONG ALTERNATIVE H5");
      end case;

      case Statcon is    --  WED
         when Thu =>
            Failed ("WRONG ALTERNATIVE I1");
         when Sun =>
            Failed ("WRONG ALTERNATIVE I2");
         when Sat =>
            Failed ("WRONG ALTERNATIVE I3");
         when Tue .. Wed =>
            null;
         when others =>
            Failed ("WRONG ALTERNATIVE I5");
      end case;

      case Dynvar is    --  THU
         when Thu =>
            null;
         when Sun =>
            Failed ("WRONG ALTERNATIVE J2");
         when Sat =>
            Failed ("WRONG ALTERNATIVE J3");
         when Tue .. Wed =>
            Failed ("WRONG ALTERNATIVE J4");
         when others =>
            Failed ("WRONG ALTERNATIVE J5");
      end case;

      case Dyncon is    --  FRI
         when Thu =>
            Failed ("WRONG ALTERNATIVE K1");
         when Sun =>
            Failed ("WRONG ALTERNATIVE K2");
         when Sat =>
            Failed ("WRONG ALTERNATIVE K3");
         when Tue .. Wed =>
            Failed ("WRONG ALTERNATIVE K4");
         when others =>
            null;
      end case;

      case Day'Succ (Dyncon) is   --  SAT
         when Thu =>
            Failed ("WRONG ALTERNATIVE L1");
         when Sun =>
            Failed ("WRONG ALTERNATIVE L2");
         when Sat =>
            null;
         when Tue .. Wed =>
            Failed ("WRONG ALTERNATIVE L4");
         when others =>
            Failed ("WRONG ALTERNATIVE L5");
      end case;
   end;

   Result;

end C54a42f;
