-- C37103A.ADA

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
-- CHECK THAT DISCRIMINANTS MAY BE BOOLEAN, CHARACTER, USER_ENUM, INTEGER,
-- DERIVED CHARACTER, DERIVED USER_ENUM, DERIVED INTEGER, AND DERIVED DERIVED
-- USER_ENUM.

-- DAT 5/18/81
-- SPS 10/25/82

with Report; use Report;

procedure C37103a is
begin
   Test ("C37103A", "MANY DIFFERENT DISCRIMINANT TYPES");
   declare
      package P1 is
         type Enum is (A, Z, Q, 'W', 'A');
      end P1;

      package P2 is
         type E2 is new P1.Enum;
      end P2;

      package P3 is
         type E3 is new P2.E2;
      end P3;

      use P1, P2, P3;
      type Int is new Integer range -3 .. 7;
      type Char is new Character;
      type R1 (D : Enum) is record
         null;
      end record;
      type R2 (D : Integer) is record
         null;
      end record;
      type R3 (D : Boolean) is record
         null;
      end record;
      type R4 (D : Character) is record
         null;
      end record;
      type R5 (D : Char) is record
         null;
      end record;
      type R6 (D : E2) is record
         null;
      end record;
      type R7 (D : E3) is record
         null;
      end record;
      type R8 (D : Int) is record
         null;
      end record;
      O1 : R1 (A)         := (D => A);
      O2 : R2 (3)         := (D => 3);
      O3 : R3 (True)      := (D => True);
      O4 : R4 (Ascii.Nul) := (D => Ascii.Nul);
      O5 : R5 ('A')       := (D => 'A');
      O6 : R6 ('A')       := (D => 'A');
      O7 : R7 (A)         := (D => A);
      O8 : R8 (2)         := (D => 2);
   begin
      if O1.D /= A or O2.D /= 3 or not O3.D or O4.D in 'A' .. 'Z' or
        O5.D /= 'A' or O6.D /= 'A' or O7.D /= A or O8.D /= 2 then
         Failed ("WRONG DISCRIMINANT VALUE");
      end if;
   end;

   Result;
end C37103a;
