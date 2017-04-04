-- C48007A.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T", CHECK THAT CONSTRAINT_ERROR IS
-- RAISED IF T IS AN UNCONSTRAINED TYPE WITH DEFAULT DISCRIMINANTS
-- (RECORD, PRIVATE OR LIMITED) AND ONE DEFAULT DISCRIMINANT VALUE DOES
-- NOT EQUAL THE CORRESPONDING VALUE SPECIFIED FOR THE ALLOCATOR'S BASE
-- TYPE.

-- EG  08/10/84

with Report;

procedure C48007a is

   use Report;

begin

   Test
     ("C48007A",
      "FOR ALLOCATORS OF THE FORM 'NEW T' CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - UNCONSTRAINED TYPE WITH " &
      "DEFAULT DISCRIMINANTS");

   declare

      type Ur (A : Integer := 1; B : Integer := 2) is record
         C : Integer := 7;
      end record;

      package P is

         type Up (A : Integer := 12; B : Integer := 13) is private;
         type Ul (A, B : Integer := 4) is limited private;

      private

         type Up (A : Integer := 12; B : Integer := 13) is record
            C : Integer := 8;
         end record;
         type Ul (A, B : Integer := 4) is record
            C : Integer := 9;
         end record;

      end P;

      use P;

      type A_Ur is access Ur (1, 9);
      type A_Up is access Up (9, 13);
      type A_Ul is access Ul (4, 9);

      Vur : A_Ur;
      Vup : A_Up;
      Vul : A_Ul;

   begin

      begin -- UR

         Vur := new Ur;
         Failed ("NO EXCEPTION RAISED - UR");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UR");

      end;

      begin -- UP

         Vup := new Up;
         Failed ("NO EXCEPTION RAISED - UP");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UP");

      end;

      begin -- UL

         Vul := new Ul;
         Failed ("NO EXCEPTION RAISED - UL");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UL");

      end;

   end;

   Result;

end C48007a;
