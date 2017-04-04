-- C46054A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR CONVERSION TO AN ACCESS SUBTYPE IF
-- THE OPERAND VALUE IS NOT NULL AND THE DISCRIMINANTS OR INDEX BOUNDS OF THE
-- DESIGNATED OBJECT DO NOT MATCH THOSE OF THE TARGET TYPE.

-- R.WILLIAMS 9/9/86

with Report; use Report;
procedure C46054a is

begin
   Test
     ("C46054A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
      "CONVERSION TO AN ACCESS SUBTYPE IF THE " &
      "OPERAND VALUE IS NOT NULL AND THE " &
      "DISCRIMINANTS OR INDEX BOUNDS OF THE " &
      "DESIGNATED OBJECT DO NOT MATCH THOSE OF " &
      "THE TARGET TYPE");

   declare
      type Rec (D : Integer) is record
         null;
      end record;

      type Acrec is access Rec;
      A : Acrec (Ident_Int (0)) := new Rec (Ident_Int (0));

      subtype Acrec3 is Acrec (Ident_Int (3));

      procedure Proc (A : Acrec) is
         I : Integer;
      begin
         I := Ident_Int (A.D);
      end Proc;

   begin
      Proc (Acrec3 (A));
      Failed ("NO EXCEPTION RAISED FOR 'ACREC3 (A)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'ACREC3 (A)'");
   end;

   declare
      type Rec (D1, D2 : Integer) is record
         null;
      end record;

      type Acrec is access Rec;

      A : Acrec (Ident_Int (3), Ident_Int (1)) :=
        new Rec (Ident_Int (3), Ident_Int (1));

      subtype Acrec13 is Acrec (Ident_Int (1), Ident_Int (3));

      procedure Proc (A : Acrec) is
         I : Integer;
      begin
         I := Ident_Int (A.D1);
      end Proc;

   begin
      Proc (Acrec13 (A));
      Failed ("NO EXCEPTION RAISED FOR 'ACREC13 (A)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'ACREC13 (A)'");
   end;

   declare
      type Arr is array (Integer range <>) of Integer;

      type Acarr is access Arr;
      A : Acarr (Ident_Int (0) .. Ident_Int (1)) :=
        new Arr'(Ident_Int (0) .. Ident_Int (1) => 0);

      subtype Acarr02 is Acarr (Ident_Int (0) .. Ident_Int (2));

      procedure Proc (A : Acarr) is
         I : Integer;
      begin
         I := Ident_Int (A'Last);
      end Proc;

   begin
      Proc (Acarr02 (A));
      Failed ("NO EXCEPTION RAISED FOR 'ACARR02 (A)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'ACARR02 (A)'");
   end;

   declare
      type Arr is array (Integer range <>, Integer range <>) of Integer;

      type Acarr is access Arr;
      A : Acarr
        (Ident_Int (1) .. Ident_Int (0),
         Ident_Int (4) .. Ident_Int (5)) :=
        new Arr'
          (Ident_Int (1) .. Ident_Int (0) =>
             (Ident_Int (4) .. Ident_Int (5) => 0));

      subtype Nacarr is
        Acarr (Ident_Int (0) .. Ident_Int (1), Ident_Int (5) .. Ident_Int (4));

      procedure Proc (A : Nacarr) is
         I : Integer;
      begin
         I := Ident_Int (A'Last (1));
      end Proc;

   begin
      Proc (Nacarr (A));
      Failed ("NO EXCEPTION RAISED FOR 'NACARR (A)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'NACARR (A)'");
   end;

   declare
      package Pkg1 is
         type Priv (D : Integer) is private;
         type Acprv is access Priv;
         subtype Acprv3 is Acprv (Ident_Int (3));

      private
         type Priv (D : Integer) is record
            null;
         end record;
      end Pkg1;

      use Pkg1;

      package Pkg2 is
         A : Acprv (Ident_Int (0)) := new Priv (Ident_Int (0));
      end Pkg2;

      use Pkg2;

      procedure Proc (A : Acprv) is
         I : Integer;
      begin
         I := Ident_Int (A.D);
      end Proc;

   begin
      Proc (Acprv3 (A));
      Failed ("NO EXCEPTION RAISED FOR 'ACPRV3 (A)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'ACPRV3 (A)'");
   end;

   Result;
end C46054a;
