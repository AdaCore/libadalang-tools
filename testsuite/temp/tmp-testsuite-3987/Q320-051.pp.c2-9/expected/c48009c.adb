-- C48009C.ADA

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
-- IS RAISED IF T IS A CONSTRAINED RECORD OR PRIVATE TYPE, (X) IS AN
-- AGGREGATE OR A VALUE OF TYPE T, AND ONE OF THE DISCRIMINANT VALUES IN
-- X:
--   1) DOES NOT EQUAL THE CORRESPONDING DISCRIMINANT VALUE FOR T.
--   2) DOES NOT EQUAL THE CORRESPONDING DISCRIMINANT VALUE SPECIFIED
--      IN THE DECLARATION OF THE ALLOCATOR'S BASE TYPE.
--   3) DOES NOT EQUAL THE CORRESPONDING DISCRIMINANT VALUE IN THE
--      ACCESS TO ACCESS CASE.

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- EG  07/05/84

with Report;

procedure C48009c is

   use Report;

begin

   Test
     ("C48009C",
      "FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - CONSTRAINED RECORD TYPES");

   declare

      type Tc0 (A, B : Integer) is record
         C : Integer range 1 .. 7;
      end record;
      subtype Tc is Tc0 (2, 3);
      type Atc is access Tc0 (2, 3);
      subtype Tc4_5 is Tc0 (Ident_Int (4), Ident_Int (5));
      Vc : Atc;

   begin

      begin
         Vc := new Tc'(102, 3, 4);
         Failed ("NO EXCEPTION RAISED - CASE 1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 1");
      end;

      begin
         Vc := new Tc4_5'(Ident_Int (4), Ident_Int (5), 1);
         Failed ("NO EXCEPTION RAISED - CASE 2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 2");
      end;

   end;

   declare

      type Ur (A : Integer) is record
         null;
      end record;
      type A_Ur is access Ur;
      subtype Ca_Ur is A_Ur (2);
      type A_Ca_Ur is access Ca_Ur;

      V : A_Ca_Ur;

   begin

      V := new Ca_Ur'(new Ur'(A => Ident_Int (3)));
      Failed ("NO EXCEPTION RAISED - CASE 3");

   exception

      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - CASE 3");

   end;

   Result;

end C48009c;
