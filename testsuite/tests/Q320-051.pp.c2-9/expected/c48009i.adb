-- C48009I.ADA

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
-- RAISED IF THE DESIGNATED TYPE FOR "NEW T'(X)" IS A CONSTRAINED ACCESS TYPE,
-- CA, T IS CA'BASE, AND A DISCRIMINANT OR INDEX VALUE OF X DOES NOT EQUAL A
-- VALUE SPECIFIED FOR CA.

-- EG  08/30/84

with Report;

procedure C48009i is

   use Report;

begin

   Test
     ("C48009I",
      "FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - ACCESS TYPE OF CONSTRAINED " &
      "ACCESS TYPE");

   declare

      type Ur (A : Integer) is record
         null;
      end record;
      type Ua is array (Integer range <>) of Integer;

      package P is
         type Up (A : Integer) is private;
         type Ul (A : Integer) is limited private;
      private
         type Up (A : Integer) is record
            null;
         end record;
         type Ul (A : Integer) is record
            null;
         end record;
      end P;

      type A_Ur is access Ur;
      type A_Ua is access Ua;
      type A_Up is access P.Up;
      type A_Ul is access P.Ul;

      type Ac_A_Ur is access A_Ur (2);
      type Ac_A_Ua is access A_Ua (2 .. 4);
      type Ac_A_Up is access A_Up (3);
      type Ac_A_Ul is access A_Ul (4);

      V_Ac_A_Ur : Ac_A_Ur;
      V_Ac_A_Ua : Ac_A_Ua;
      V_Ac_A_Up : Ac_A_Up;
      V_Ac_A_Ul : Ac_A_Ul;

   begin

      begin
         V_Ac_A_Ur := new A_Ur'(new Ur (3));
         Failed ("NO EXCEPTION RAISED - UR");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UR");
      end;

      begin
         V_Ac_A_Ua := new A_Ua'(new Ua (3 .. 5));
         Failed ("NO EXCEPTION RAISED - UA");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UA");
      end;

      begin
         V_Ac_A_Up := new A_Up'(new P.Up (Ident_Int (4)));
         Failed ("NO EXCEPTION RAISED - UP");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UP");
      end;

      begin
         V_Ac_A_Ul := new A_Ul'(new P.Ul (Ident_Int (5)));
         Failed ("NO EXCEPTION RAISED - UL");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UL");
      end;

   end;

   Result;

end C48009i;
