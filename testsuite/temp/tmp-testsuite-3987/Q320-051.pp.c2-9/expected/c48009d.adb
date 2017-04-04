-- C48009D.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT CONSTRAINT_ERROR
-- IS RAISED IF T IS AN UNCONSTRAINED ARRAY TYPE WITH INDEX SUBTYPE(S)
-- S,
--   1) X HAS TOO MANY VALUES FOR S;
--   2) A NAMED NON-NULL BOUND OF X LIES OUTSIDE S'S RANGE;
--   3) THE BOUND'S OF X ARE NOT EQUAL TO BOUNDS SPECIFIED FOR THE
--      ALLOCATOR'S DESIGNATED BASE TYPE. (THEY ARE EQUAL TO THE BOUNDS
--      SPECIFIED FOR T).

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- JBG 03/03/83
-- EG  07/05/84
-- PWN 11/30/94 REMOVED TEST ILLEGAL IN ADA 9X.
-- KAS 11/14/95 FOR SLIDING ASSIGNMENT, CHANGED FAIL TO COMMENT ON LANGUAGE
-- KAS 12/02/95 INCLUDED SECOND CASE
-- PWN 05/03/96 Enforced Ada 95 sliding rules

with Report;

procedure C48009d is

   use Report;

begin

   Test
     ("C48009D",
      "FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - UNCONSTRAINED ARRAY TYPES");
   declare

      subtype Two is Integer range 1 .. 2;
      subtype Twon is Integer range Ident_Int (1) .. Ident_Int (2);
      type Ua is array (Integer range <>) of Integer;
      type Td is array (Two range <>) of Integer range 1 .. 7;
      type Tdn is array (Twon range <>) of Integer range 1 .. 7;
      type Atd is access Td;
      type Atdn is access Tdn;
      type A_Ua is access Ua;
      type A_Ca is access Ua (3 .. 4);
      type A_Can is access Ua (4 .. 3);
      Vd      : Atd;
      Vdn     : Atdn;
      V_A_Ca  : A_Ca;
      V_A_Can : A_Can;

   begin

      begin
         Vd := new Td'(3, 4, 5);
         Failed ("NO EXCEPTION RAISED - CASE 1A");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 1A");
      end;

      begin
         Vdn := new Tdn'(3, 4, 5);
         Failed ("NO EXCEPTION RAISED - CASE 1B");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 1B");
      end;

      begin
         Vd := new Td'(Ident_Int (0) .. 2 => 6);
         Failed ("NO EXCEPTION RAISED - CASE 2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 2");
      end;

      begin
         V_A_Ca := new Ua'(2 .. 3 => 3);
         Comment ("ADA 95 SLIDING ASSIGNMENT - CASE 3A");
      exception
         when Constraint_Error =>
            Failed ("ADA 83 NON SLIDING ASSIGNMENT - CASE 3A");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3A");
      end;

      begin
         V_A_Can := new Ua'(Ident_Int (3) .. Ident_Int (2) => 3);
         Comment ("ADA 95 SLIDING ASSIGNMENT - CASE 3B");
      exception
         when Constraint_Error =>
            Failed ("ADA 83 NON SLIDING ASSIGNMENT - CASE 3B");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3B");
      end;

   end;

   Result;

end C48009d;
