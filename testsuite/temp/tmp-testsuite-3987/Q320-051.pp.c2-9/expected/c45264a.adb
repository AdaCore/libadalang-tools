-- C45264A.ADA

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
-- CHECK THAT EQUALITY COMPARISONS YIELD CORRECT RESULTS FOR ONE
-- DIMENSIONAL AND MULTI-DIMENSIONAL ARRAY TYPES.
-- CASE THAT CHECKS THAT TWO NULL ARRAYS OF THE SAME TYPE ARE
-- ALWAYS EQUAL.

-- PK  02/21/84
-- EG  05/30/84

with Report; use Report;

procedure C45264a is

   subtype Int is Integer range 1 .. 3;

begin

   Test
     ("C45264A",
      "CHECK THAT EQUALITY COMPARISONS YIELD CORRECT " &
      "RESULTS FOR ONE DIMENSIONAL AND MULTI-" &
      "DIMENSIONAL ARRAY TYPES");

   declare

      type A1 is array (Int range <>) of Integer;

   begin

      if A1'(1 .. Ident_Int (2) => Ident_Int (1)) /=
        A1'(Ident_Int (2) .. 3 => Ident_Int (1))
      then
         Failed ("A1 - ARRAYS NOT EQUAL");
      end if;

   exception

      when others =>
         Failed ("A1 - EXCEPTION RAISED");

   end;

   declare

      type A2 is array (Int range <>, Int range <>) of Integer;

   begin
      if A2'
          (1 .. Ident_Int (2) =>
             (Ident_Int (3) .. Ident_Int (2) => Ident_Int (1))) /=
        A2'
          (Ident_Int (2) .. 3 =>
             (Ident_Int (2) .. Ident_Int (1) => Ident_Int (1)))
      then
         Failed ("A2 - ARRAYS NOT EQUAL");
      end if;

   exception

      when others =>
         Failed ("A2 - EXCEPTION RAISED");

   end;

   declare

      type A3 is array (Int range <>, Int range <>, Int range <>) of Integer;

   begin

      if A3'
          (1 .. Ident_Int (2) =>
             (Ident_Int (1) .. Ident_Int (3) =>
                (Ident_Int (3) .. Ident_Int (2) => Ident_Int (1)))) /=
        A3'
          (Ident_Int (1) .. 3 =>
             (Ident_Int (2) .. Ident_Int (1) =>
                (Ident_Int (1) .. Ident_Int (2) => Ident_Int (1))))
      then
         Failed ("A3 - ARRAYS NOT EQUAL");
      end if;

   exception

      when others =>
         Failed ("A3 - EXCEPTION RAISED");

   end;

   Result;

end C45264a;
