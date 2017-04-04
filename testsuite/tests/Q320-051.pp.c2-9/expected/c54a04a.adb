-- C54A04A.ADA

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
-- CHECK THAT PRIVATE (DISCRETE) TYPES MAY BE USED IN CASE EXPRESSIONS
--     WITHIN THE DEFINING PACKAGE.

-- DAT 1/29/81

with Report;
procedure C54a04a is

   use Report;

   package P is

      type T is private;
      type Lt is limited private;

   private

      type T is ('Z', X);
      type Lt is new Integer range 0 .. 1;

   end P;

   Vt  : P.T;
   Vlt : P.Lt;

   package body P is

   begin
      Test
        ("C54A04A",
         "PRIVATE DISCRETE TYPES MAY APPEAR IN " &
         "CASE EXPRESSIONS IN PACKAGE BODY");

      Vt  := 'Z';
      Vlt := Lt (Ident_Int (1));

      case Vt is
         when X =>
            Failed ("WRONG CASE 1");
         when 'Z' =>
            null; -- OK
      end case;

      case Vlt is
         when 1 =>
            null; -- OK
         when 0 =>
            Failed ("WRONG CASE 2");
      end case;
   end P;

begin

   -- TEST CALLED FROM PACKAGE BODY, ALREADY ELABORATED.

   Result;
end C54a04a;
