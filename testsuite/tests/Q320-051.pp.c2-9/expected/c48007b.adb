-- C48007B.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T", CHECK THAT CONSTRAINT_ERROR IS RAISED
-- IF T IS A CONSTRAINED TYPE WITH DISCRIMINANTS (RECORD, PRIVATE OR LIMITED)
-- AND AT LEAST ONE DISCRIMINANT VALUE SPECIFIED FOR T DOES NOT EQUAL THE
-- CORRESPONDING VALUE SPECIFIED FOR THE ALLOCATOR'S BASE TYPE.

-- EG  08/10/84

with Report;

procedure C48007b is

   use Report;

begin

   Test
     ("C48007B",
      "FOR ALLOCATORS OF THE FORM 'NEW T' CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - CONSTRAINED TYPE WITH " &
      "DISCRIMINANT");

   declare

      type Ur (A, B : Integer) is record
         C : Integer;
      end record;

      package P is

         type Up (A, B : Integer) is private;
         type Ul (A, B : Integer) is limited private;

      private

         type Up (A, B : Integer) is record
            C : Integer;
         end record;
         type Ul (A, B : Integer) is record
            C : Integer;
         end record;

      end P;

      use P;

      subtype Cr is Ur (1, 2);
      subtype Cp is Up (12, 13);
      subtype Cl is Ul (4, 4);

      type A_Ur is access Ur (1, 9);
      type A_Up is access Up (9, 13);
      type A_Ul is access Ul (4, 9);

      Vur : A_Ur;
      Vup : A_Up;
      Vul : A_Ul;

   begin

      begin -- CR

         Vur := new Cr;
         Failed ("NO EXCEPTION RAISED - CR");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CR");

      end;

      begin -- CP

         Vup := new Cp;
         Failed ("NO EXCEPTION RAISED - CP");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CP");

      end;

      begin -- CL

         Vul := new Cl;
         Failed ("NO EXCEPTION RAISED - CL");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CL");

      end;

   end;

   Result;

end C48007b;
