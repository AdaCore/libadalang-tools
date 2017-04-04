-- C48009B.ADA

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
-- IS RAISED IF T IS AN UNCONSTRAINED RECORD OR PRIVATE TYPE, (X) IS AN
-- AGGREGATE OR A VALUE OF TYPE T, AND ONE OF THE DISCRIMINANT VALUES IN
-- X:
--   1) DOES NOT SATISFY THE RANGE CONSTRAINT FOR THE CORRESPONDING
--      DISCRIMINANT OF T.
--   2) DOES NOT EQUAL THE DISCRIMINANT VALUE SPECIFIED IN THE
--      DECLARATION OF THE ALLOCATOR'S BASE TYPE.
--   3) A DISCRIMINANT VALUE IS COMPATIBLE WITH A DISCRIMINANT'S SUBTYPE
--      BUT DOES NOT PROVIDE A COMPATIBLE INDEX OR DISCRIMINANT
--      CONSTRAINT FOR A SUBCOMPONENT DEPENDENT ON THE DISCRIMINANT.

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- JBG 03/02/83
-- EG  07/05/84

with Report;

procedure C48009b is

   use Report;

begin

   Test
     ("C48009B",
      "FOR ALLOCATORS OF THE FORM 'NEW T '(X)', " &
      "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - UNCONSTRAINED RECORD AND " &
      "PRIVATE TYPES");

   declare

      subtype I1_7 is Integer range Ident_Int (1) .. Ident_Int (7);
      subtype I1_10 is Integer range Ident_Int (1) .. Ident_Int (10);
      subtype I2_9 is Integer range Ident_Int (2) .. Ident_Int (9);

      type Rec (A : I2_9) is record
         null;
      end record;

      type Arr is array (I2_9 range <>) of Integer;

      type T_Rec (C : I1_10) is record
         D : Rec (C);
      end record;

      type T_Arr (C : I1_10) is record
         D : Arr (2 .. C);
         E : Arr (C .. 9);
      end record;

      type T_Rec_Rec (A : I1_10) is record
         B : T_Rec (A);
      end record;

      type T_Rec_Arr (A : I1_10) is record
         B : T_Arr (A);
      end record;

      type Tb (A : I1_7) is record
         R : Integer;
      end record;

      type A_T_Rec_Rec is access T_Rec_Rec;
      type A_T_Rec_Arr is access T_Rec_Arr;
      type Atb is access Tb;
      type Actb is access Tb (3);

      Va_T_Rec_Rec : A_T_Rec_Rec;
      Va_T_Rec_Arr : A_T_Rec_Arr;
      Vb           : Atb;
      Vcb          : Actb;

      package P is
         type Priv (A : I1_10) is private;
         Cons_Priv : constant Priv;
      private
         type Priv (A : I1_10) is record
            R : Integer;
         end record;
         Cons_Priv : constant Priv := (2, 3);
      end P;

      use P;

      type A_Priv is access P.Priv;
      type A_Cpriv is access P.Priv (3);

      Vp  : A_Priv;
      Vcp : A_Cpriv;

      function Alloc1 (X : P.Priv) return A_Cpriv is
      begin
         if Equal (1, 1) then
            return new P.Priv'(X);
         else
            return null;
         end if;
      end Alloc1;
      function Alloc2 (X : Tb) return Actb is
      begin
         if Equal (1, 1) then
            return new Tb'(X);
         else
            return null;
         end if;
      end Alloc2;

   begin

      begin     -- B1
         Vb := new Tb'(A => Ident_Int (0), R => 1);
         Failed ("NO EXCEPTION RAISED - CASE 1A");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 1A");
      end;

      begin
         Vb := new Tb'(A => 8, R => 1);
         Failed ("NO EXCEPTION RAISED - CASE 1B");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 1B");
      end; -- B1

      begin     -- B2
         Vcb := new Tb'(2, 3);
         Failed ("NO EXCEPTION RAISED - CASE 2A");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 2A");
      end;

      begin
         if Alloc2 ((Ident_Int (4), 3)) = null then
            Failed ("IMPOSSIBLE - CASE 2B");
         end if;
         Failed ("NO EXCEPTION RAISED - CASE 2B");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 2B");
      end;

      begin

         if Alloc1 (Cons_Priv) = null then
            Failed ("IMPOSSIBLE - CASE 2C");
         end if;
         Failed ("NO EXCEPTION RAISED - CASE 2C");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 2C");

      end; -- B2

      begin     -- B3

         Va_T_Rec_Rec := new T_Rec_Rec'(1, (1, (A => 1)));
         Failed ("NO EXCEPTION RAISED - CASE 3A");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3A");

      end;

      begin

         Va_T_Rec_Rec := new T_Rec_Rec'(10, (10, (A => 10)));
         Failed ("NO EXCEPTION RAISED - CASE 3B");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3B");

      end;

      begin

         Va_T_Rec_Arr := new T_Rec_Arr'(1, (1, (others => 1), (others => 2)));
         Failed ("NO EXCEPTION RAISED - CASE 3C");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3C");

      end;

      begin

         Va_T_Rec_Arr :=
           new T_Rec_Arr'(10, (10, (others => 1), (others => 2)));
         Failed ("NO EXCEPTION RAISED - CASE 3D");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE 3D");

      end;

   end;

   Result;

end C48009b;
