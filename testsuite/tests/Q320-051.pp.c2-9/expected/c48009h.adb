-- C48009H.ADA

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
-- RAISED IF T IS AN (UNCONSTRAINED) ACCESS TYPE, THE DESIGNATED TYPE FOR
-- T'BASE IS CONSTRAINED, AND THE OBJECT DESIGNATED BY X DOES NOT HAVE
-- DISCRIMINANTS OR INDEX BOUNDS THAT EQUAL THE CORRESPONDING VALUES FOR
-- T'S DESIGNATED TYPE.

-- EG  08/30/84

with Report;

procedure C48009h is

   use Report;

begin

   Test
     ("C48009H",
      "FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - UNCONSTRAINED ACCESS TYPE OF A " &
      "CONSTRAINED TYPE");

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

      type A_Cr is access Ur (Ident_Int (2));
      type A_Ca is access Ua (2 .. Ident_Int (4));
      type A_Cp is access P.Up (3);
      type A_Cl is access P.Ul (4);

      type Aa_Cr is access A_Cr;
      type Aa_Ca is access A_Ca;
      type Aa_Cp is access A_Cp;
      type Aa_Cl is access A_Cl;

      V_Aa_Cr : Aa_Cr;
      V_Aa_Ca : Aa_Ca;
      V_Aa_Cp : Aa_Cp;
      V_Aa_Cl : Aa_Cl;

   begin

      begin
         V_Aa_Cr := new A_Cr'(new Ur (3));
         Failed ("NO EXCEPTION RAISED - CR");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CR");
      end;

      begin
         V_Aa_Ca := new A_Ca'(new Ua (Ident_Int (3) .. 5));
         Failed ("NO EXCEPTION RAISED - CA");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CA");
      end;

      begin
         V_Aa_Cp := new A_Cp'(new P.Up (Ident_Int (4)));
         Failed ("NO EXCEPTION RAISED - CP");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CP");
      end;

      begin
         V_Aa_Cl := new A_Cl'(new P.Ul (5));
         Failed ("NO EXCEPTION RAISED - CL");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CL");
      end;

   end;

   Result;

end C48009h;
