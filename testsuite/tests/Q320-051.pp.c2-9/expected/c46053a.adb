-- C46053A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR CONVERSION TO A CONSTRAINED
-- RECORD, PRIVATE, OR LIMITED PRIVATE SUBTYPE IF THE DISCRIMINANTS OF THE
-- TARGET SUBTYPE DO NOT EQUAL THOSE OF THE OPERAND.

-- R.WILLIAMS 9/9/86

with Report; use Report;
procedure C46053a is

begin
   Test
     ("C46053A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
      "CONVERSION TO A CONSTRAINED RECORD, " &
      "PRIVATE, OR LIMITED PRIVATE SUBTYPE IF " &
      "THE DISCRIMINANTS OF THE TARGET SUBTYPE DO " &
      "NOT EQUAL THOSE OF THE OPERAND");

   declare
      type Rec (D : Integer) is record
         null;
      end record;

      subtype Rec3 is Rec (Ident_Int (3));
      R : Rec (Ident_Int (1));

      procedure Proc (R : Rec) is
         I : Integer;
      begin
         I := Ident_Int (R.D);
      end Proc;

   begin
      Proc (Rec3 (R));
      Failed ("NO EXCEPTION RAISED FOR 'REC3 (R)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'REC3 (R)'");
   end;

   declare
      package Pkg1 is
         type Priv (D : Integer) is private;
         subtype Priv3 is Priv (Ident_Int (3));
      private
         type Priv (D : Integer) is record
            null;
         end record;
      end Pkg1;

      use Pkg1;

      package Pkg2 is
         P : Priv (Ident_Int (0));
      end Pkg2;

      use Pkg2;

      procedure Proc (P : Priv) is
         I : Integer;
      begin
         I := Ident_Int (P.D);
      end Proc;

   begin
      Proc (Priv3 (P));
      Failed ("NO EXCEPTION RAISED FOR 'PRIV3 (P)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'PRIV3 (P)'");
   end;

   declare
      package Pkg1 is
         type Lim (D : Integer) is limited private;
         subtype Lim3 is Lim (Ident_Int (3));
      private
         type Lim (D : Integer) is record
            null;
         end record;
      end Pkg1;

      use Pkg1;

      package Pkg2 is
         L : Lim (Ident_Int (0));
         I : Integer;
      end Pkg2;

      use Pkg2;

      procedure Proc (L : Lim) is
         I : Integer;
      begin
         I := Ident_Int (L.D);
      end Proc;

   begin
      Proc (Lim3 (L));
      Failed ("NO EXCEPTION RAISED FOR 'LIM3 (L)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'LIM3 (L)'");
   end;

   Result;
end C46053a;
