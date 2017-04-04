-- C48009F.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT CONSTRAINT_ERROR IS
-- RAISED IF T IS A CONSTRAINED OR UNCONSTRAINED MULTI-DIMENSIONAL ARRAY
-- TYPE AND ALL COMPONENTS OF X DO NOT HAVE THE SAME LENGTH OR BOUNDS.

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- JBG 03/03/83
-- EG  07/05/84

with Report;

procedure C48009f is

   use Report;

begin

   Test
     ("C48009F",
      "FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "X IS AN ILL-FORMED MULTIDIMENSIONAL AGGREGATE");

   declare

      type Tg00 is array (4 .. 2) of Integer;
      type Tg10 is array (1 .. 2) of Integer;
      type Tg20 is array (Integer range <>) of Integer;

      type Tg0 is array (3 .. 2) of Tg00;
      type Tg1 is array (1 .. 2) of Tg10;
      type Tg2 is array (Integer range <>) of Tg20 (1 .. 3);

      type Atg0 is access Tg0;
      type Atg1 is access Tg1;
      type Atg2 is access Tg2;

      Vg0 : Atg0;
      Vg1 : Atg1;
      Vg2 : Atg2;

   begin

      begin
         Vg0 := new Tg0'(5 .. 4 => (3 .. 1 => 2));
         Failed ("NO EXCEPTION RAISED - CASE 0");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 0");
      end;

      begin
         Vg1 := new Tg1'((1, 2), (3, 4, 5));
         Failed ("NO EXCEPTION RAISED - CASE 1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 1");
      end;

      begin
         Vg2 := new Tg2'(1 => (1 .. 2 => 7), 2 => (1 .. 3 => 7));
         Failed ("NO EXCEPTION RAISED - CASE 2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 2");
      end;

   end;

   Result;

end C48009f;
