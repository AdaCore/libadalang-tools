-- C48009J.ADA

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
-- IS RAISED IF T IS AN UNCONSTRAINED ACCESS TYPE, ITS DESIGNATED TYPE
-- IS ALSO UNCONSTRAINED, AND A DISCRIMINANT VALUE FOR X LIES OUTSIDE
-- THE RANGE OF THE CORRESPONDING DISCRIMINANT SPECIFICATION FOR THE
-- DESIGNATED TYPE, OR A NON-NULL INDEX BOUND LIES OUTSIDE THE RANGE OF
-- AN INDEX SUBTYPE OF THE DESIGNATED TYPE.

-- EG  08/30/84

with Report;

procedure C48009j is

   use Report;

begin

   Test
     ("C48009J",
      "FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - ACCESS TYPE OF UNCONSTRAINED " &
      "ACCESS TYPE");

   declare

      type Int is range 1 .. 5;

      type Ur (A : Int) is record
         null;
      end record;
      type Ua is array (Int range <>) of Integer;

      package P is
         type Up (A : Int) is private;
         type Ul (A : Int) is limited private;
      private
         type Up (A : Int) is record
            null;
         end record;
         type Ul (A : Int) is record
            null;
         end record;
      end P;

      type A_Ur is access Ur;
      type A_Ua is access Ua;
      type A_Up is access P.Up;
      type A_Ul is access P.Ul;

      type Aa_Ur is access A_Ur;
      type Aa_Ua is access A_Ua;
      type Aa_Up is access A_Up;
      type Aa_Ul is access A_Ul;

      V_Aa_Ur : Aa_Ur;
      V_Aa_Ua : Aa_Ua;
      V_Aa_Up : Aa_Up;
      V_Aa_Ul : Aa_Ul;

   begin

      begin
         V_Aa_Ur := new A_Ur'(new Ur (Int (Ident_Int (6))));
         Failed ("NO EXCEPTION RAISED - UR");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UR");
      end;

      begin
         V_Aa_Ua := new A_Ua'(new Ua (4 .. 7));
         Failed ("NO EXCEPTION RAISED - UA");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UA");
      end;

      begin
         V_Aa_Up := new A_Up'(new P.Up (0));
         Failed ("NO EXCEPTION RAISED - UP");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UP");
      end;

      begin
         V_Aa_Ul := new A_Ul'(new P.Ul (Int (Ident_Int (0))));
         Failed ("NO EXCEPTION RAISED - UL");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UL");
      end;

   end;

   Result;

end C48009j;
