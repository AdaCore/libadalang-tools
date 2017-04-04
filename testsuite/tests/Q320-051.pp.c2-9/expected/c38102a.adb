-- C38102A.ADA

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
-- CHECK THAT AN INCOMPLETE TYPE DECLARATION CAN BE GIVEN FOR ANY TYPE. FULL
-- DECLARATIONS FOR INTEGER, ENUMERATION, CONSTRAINED AND UNCONSTRAINED ARRAYS,
-- RECORDS WITHOUT DISCRIMINANTS, AN ACCESS TYPE, OR TYPES DERIVED FROM ANY OF
-- THE ABOVE.

-- (FLOAT, FIXED, TASKS AND RECORDS WITH DISCRIMINANTS ARE CHECKED IN OTHER
-- TESTS).

-- DAT 3/24/81
-- SPS 10/25/82
-- SPS 2/17/82

with Report; use Report;

procedure C38102a is
begin
   Test ("C38102A", "ANY TYPE MAY BE INCOMPLETE");

   declare

      type X1;
      type X2;
      type X3;
      type X4;
      type X5;
      type X6;
      type X7;
      type X8;

      type D1;
      type D2;
      type D3;
      type D4;
      type D5;
      type D6;

      type X1 is range 1 .. 10;
      type X2 is (True, False, Maybe, Green);
      type X3 is array (1 .. 3) of String (1 .. 10);
      type X4 is array (Natural range <>) of X3;
      type Ar1 is array (X2) of X3;
      type X5 is record
         C1 : X4 (1 .. 3);
         C2 : Ar1;
      end record;
      type X6 is access X8;
      type X7 is access X6;
      type X8 is access X6;

      type D1 is new X1;
      type D2 is new X2;
      type D3 is new X3;
      type D4 is new X4;
      type D5 is new X5;
      subtype D7 is X7;
      subtype D8 is X8;
      type D6 is access D8;

      package P is

         type X1;
         type X2;
         type X3;
         type X4;
         type X5;
         type X6;
         type X7 is private;
         type X8 is limited private;

         type D1;
         type D2;
         type D3;
         type D4;
         type D5;
         type D6;

         type X1 is range 1 .. 10;
         type X2 is (True, False, Maybe, Green);
         type X3 is array (1 .. 3) of String (1 .. 10);
         type X4 is array (Natural range <>) of X3;
         type Ar1 is array (X2) of X3;
         type X5 is record
            C1 : X4 (1 .. 3);
            C2 : Ar1;
         end record;
         type X6 is access X8;

         type D1 is range 1 .. 10;
         type D2 is new X2;
         type D3 is new X3;
         type D4 is new X4;
         type D5 is new X5;
         type D6 is new X6;
         subtype D7 is X7;
         subtype D8 is X8;
         type D9 is access D8;

         Vx7 : constant X7;

      private

         type X7 is record
            C1 : X1;
            C3 : X3;
            C5 : X5;
            C6 : X6;
            C8 : D9;
         end record;

         V3 : X3 := (X3'Range => "ABCDEFGHIJ");
         type A7 is access X7;
         type X8 is array (V3'Range) of A7;

         Vx7 : constant X7 :=
           (3, V3, ((1 .. 3 => V3), (True .. Green => V3)), null, new D8);
      end P;
      use P;

      Vd7 : P.D7;

      package body P is
      begin
         Vd7 := D7 (Vx7);
      end P;

   begin
      if Vx7 /= P.X7 (Vd7) then
         Failed ("WRONG VALUE SOMEWHERE");
      end if;
   end;

   Result;
end C38102a;
