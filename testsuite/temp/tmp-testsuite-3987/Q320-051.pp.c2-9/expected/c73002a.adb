-- C73002A.ADA

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
-- CHECK THAT THE STATEMENTS IN A PACKAGE BODY ARE EXECUTED AFTER THE
--    ELABORATION OF THE DECLARATIONS (IN SPEC AND IN BODY).

-- RM 05/15/81
-- JBG 9/21/83

with Report;
procedure C73002a is

   use Report;

begin

   Test
     ("C73002A",
      "CHECK: EXECUTION OF STATEMENTS IN A PACKAGE " &
      "BODY FOLLOWS ELABORATION OF THE DECLARATIONS");

   declare

      package P1 is

         A : Integer := Ident_Int (7);

         package P2 is
            B : Integer := Ident_Int (11);
         end P2;

      end P1;

      package body P1 is               --   A  AA   B  BB

         Aa : Integer := Ident_Int (7); --   7   7  11 (11)

         package body P2 is
            Bb : Integer := Ident_Int (11);--  7  11  11
         begin

            B  := 2 * B;             --   7   7  22  11
            Bb := 2 * Bb;             --   7   7  22  22
            A  := 5 * A;             --  35   7  22  22
            Aa := 2 * Aa;             --  35  14  22  22

            if Bb /= 22 or Aa /= 14 or A /= 35 or B /= 22 then
               Failed ("ASSIGNED VALUES INCORRECT  -  1");
            end if;

         end P2;

      begin

         A  := A + 20;              --  55  14  22  22
         Aa := Aa + 20;              --  55  34  22  22

         if Aa /= 34 or A /= 55 or P2.B /= 22 then
            Failed ("ASSIGNED VALUES INCORRECT  -  2");
         end if;

      end P1;

      use P1;
      use P2;

   begin

      if A /= 55 or B /= 22 then
         Failed ("ASSIGNED VALUES INCORRECT  -  3");
      end if;

   end;

   Result;

end C73002a;
