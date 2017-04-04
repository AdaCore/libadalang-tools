-- C48009E.ADA

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
-- RAISED IF T IS A CONSTRAINED ARRAY TYPE AND:
--   1) A NAMED NULL OR NON-NULL BOUND FOR X DOES NOT EQUAL THE
--      CORRESPONDING BOUND FOR T;
--   2) A BOUND OF T DOES NOT EQUAL THE CORRESPONDING VALUE SPECIFIED IN
--      THE DECLARATION OF THE ALLOCATOR'S BASE TYPE;
--   3) A POSITIONAL AGGREGATE DOES NOT HAVE THE NUMBER OF COMPONENTS
--      REQUIRED BY T OR BY THE ALLOCATOR'S BASE TYPE.

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- JBG 03/03/83
-- EG  07/05/84
-- PWN 11/30/94 REMOVED TEST ILLEGAL IN ADA 9X. KAS 11/14/95 CHANGED FAILURE AT
-- SLIDING ASSIGNMENT TO COMMENT ON LANGUAGE KAS 11/30/95 REINSTRUMENTED CASES
-- TO SELECT LANGUAGE SEMANTICS PWN 05/03/96 Enforced Ada 95 sliding rules
-- PWN 10/24/96 Adjusted expected results for Ada 95. TMB 11/19/96 BACKED OUT
-- CHANGE FOR SLIDING WITH ACCESS TYPES MRM 12/16/96 Removed problem code from
-- withdrawn version of test, and
--              implemented a dereference-index check to ensure Ada95
--              required behavior.
-- PWB.CTA 03/07/97 Restored checks from 1.11 in 2 cases where sliding does
--                  not occur
with Report;

procedure C48009e is

   use Report;

begin

   Test
     ("C48009E",
      "FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - CONSTRAINED ARRAY TYPES");
   declare

      type Ua is array (Integer range <>) of Integer;
      type Ca3_2 is array (3 .. 2) of Integer;
      type Sa1_3 is array (1 .. 3) of Integer;
      type Na1_3 is array (1 .. Ident_Int (3)) of Integer;
      subtype Ca2_6 is Ua (2 .. 6);
      subtype Ca1_4 is Ua (1 .. 4);
      subtype Ca1_6 is Ua (1 .. 6);
      subtype Ca4_1 is Ua (4 .. 1);
      subtype Ca4_2 is Ua (4 .. 2);

      type A_Ca3_2 is access Ca3_2;
      type A_Sa1_3 is access Sa1_3;
      type A_Na1_3 is access Na1_3;
      type A_Ca1_5 is access Ua (1 .. 5);
      type A_Ca4_2 is access Ca4_2;

      V_A_Ca3_2 : A_Ca3_2;
      V_A_Sa1_3 : A_Sa1_3;
      V_A_Na1_3 : A_Na1_3;
      V_A_Ca1_5 : A_Ca1_5;

      function Alloc1 (X : Ca2_6) return A_Ca1_5 is
      begin
         if Equal (1, 1) then
            return new Ca2_6'(X);
         else
            return null;
         end if;
      end Alloc1;
      function Alloc2 (X : Ca4_1) return A_Ca4_2 is
      begin
         if Equal (1, 1) then
            return new Ca4_1'(X);
         else
            return null;
         end if;
      end Alloc2;

   begin

      begin
         V_A_Ca3_2 := new Ca3_2'(Ident_Int (4) .. Ident_Int (2) => 5);
         Failed ("NO EXCEPTION RAISED - CASE 1A");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 1A");
      end;

      begin
         V_A_Na1_3 := new Na1_3'(1 .. Ident_Int (2) => 4);
         Failed ("NO EXCEPTION RAISED - CASE 1B");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 1B");
      end;

      begin
         -- note that ALLOC1 returns A_CA1_5, so both (1) and (5) are valid
         -- index references!
         if Alloc1 ((2 .. 6 => 2)) (5) /= 2 then
            Failed ("Wrong Value Returned - CASE 2A");
         elsif Alloc1 ((2 .. 6 => 3)) (1) /= 3 then
            Failed ("Unlikely Index Case - CASE 2A");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED - CASE 2A");
      end;

      begin
         if Alloc2 ((4 .. 1 => 3)) = null then
            Failed ("IMPOSSIBLE - CASE 2B");
         end if;
         Comment ("ADA 95 SLIDING ASSIGNMENT");
      exception
         when Constraint_Error =>
            Failed ("ADA 83 NON-SLIDING ASSIGNMENT");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 2B");
      end;

      begin
         V_A_Sa1_3 := new Sa1_3'(1, 2);
         Failed ("NO EXCEPTION RAISED - CASE 3A");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3A");
      end;

      begin
         V_A_Sa1_3 := new Sa1_3'(3, 4, 5, 6);
         Failed ("NO EXCEPTION RAISED - CASE 3B");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3B");
      end;

      begin
         V_A_Na1_3 := new Na1_3'(1, 2);
         Failed ("NO EXCEPTION RAISED - CASE 3C");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3C");
      end;

      begin -- SATISFIES T BUT NOT BASE TYPE.
         V_A_Ca1_5 := new Ca1_4'(1, 2, 3, 4);
         Failed ("NO EXCEPTION RAISED - CASE 3D");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3D");
      end;

      begin -- SATISFIES T BUT NOT BASE TYPE.
         V_A_Ca1_5 := new Ca1_6'(1, 2, 3, 4, 5, 6);
         Failed ("NO EXCEPTION RAISED - CASE 3E");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3E");
      end;

      begin -- SATISFIES BASE TYPE BUT NOT T.
         V_A_Ca1_5 := new Ca1_4'(1, 2, 3, 4, 5);
         Failed ("NO EXCEPTION RAISED - CASE 3F");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3F");
      end;

      begin -- SATISFIES BASE TYPE BUT NOT T.
         V_A_Ca1_5 := new Ca1_6'(1, 2, 3, 4, 5);
         Failed ("NO EXCEPTION RAISED - CASE 3G");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3G");
      end;

   end;

   Result;

end C48009e;
