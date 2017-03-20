-- C37008B.ADA

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
-- CHECK THAT NO CONSTRAINT ERROR IS RAISED FOR AN UNUSED TYPE
-- DECLARATION WITH AN INVALID DEFAULT VALUE

-- JBG 9/11/81
-- SPS 10/25/82

with Report; use Report;
procedure C37008b is
begin
   Test
     ("C37008B",
      "CHECK THAT INVALID DEFAULT RECORD" &
      " COMPONENT INITIALIZATIONS DO NOT RAISE" &
      " CONSTRAINT_ERROR");

   begin
      declare
         type R1 is record
            C1 : Integer range 1 .. 5 := 0;
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 1");
   end;

   begin
      declare
         type R is record
            C : Character range 'A' .. 'Y' := 'Z';
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 1A");
   end;

   begin
      declare
         type R2 is record
            C2 : Boolean range False .. False := True;
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 2");
   end;

   begin
      declare
         type E is (E1, E2, E3);
         type R is record
            C : E range E2 .. E3 := E1;
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 2A");
   end;

   begin
      declare
         type R3 is record
            C3 : Integer range 1 .. 5;
         end record;
         type R3a is record
            C3a : R3 := (others => 6);
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 3");
   end;

   begin
      declare
         type Arr is array (1 .. 3) of Integer range 8 .. 9;
         type R4 is record
            C4 : Arr := (1 => 8, 2 => 9, 3 => 10);
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 4");
   end;

   begin
      declare
         type A is array (Natural range <>) of Integer range 1 .. 5;

         type Aa is access A;

         type R5 is record
            C5 : Aa := new A'(4, 5, 6);
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 5");
   end;

   begin
      declare
         type A is array (Natural range <>) of Integer range 1 .. 5;

         type Aa is access A (1 .. 3);

         type R6 is record
            C6 : Aa := new A'(4, 4, 4, 4);
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 6");
   end;

   begin
      declare
         type Ai is access Integer range 6 .. 8;

         type R7 is record
            C7 : Ai := new Integer'(5);
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 7");
   end;

   begin
      declare
         type Ua is array (Natural range <>) of Integer range 3 .. 5;

         subtype Ca is Ua (7 .. 8);

         type R8 is record
            C8 : Ca := (6 .. 8 => 4);
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 8");
   end;

   begin
      declare
         type Ua is
           array (Natural range <>) of Integer range 3 .. Ident_Int (5);

         type R9 is record
            C9 : Ua (11 .. 11) := (11 => 6);
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 9");
   end;

   begin
      declare
         type A is
           array (Natural range <>) of Integer range 1 .. Ident_Int (5);

         type Aa is access A;

         type R10 is record
            C10 : Aa := new A'(4, 5, 6);
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 10");
   end;

   begin
      declare
         type A is array (Natural range <>) of Integer range 1 .. 5;

         type Aa is access A (Ident_Int (1) .. Ident_Int (3));

         type R11 is record
            C11 : Aa := new A'(4, 4, 4, 4);
         end record;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 11");
   end;

   Result;
end C37008b;
