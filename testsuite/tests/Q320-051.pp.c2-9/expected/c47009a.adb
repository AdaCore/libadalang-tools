-- C47009A.ADA

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
-- OBJECTIVE:
--     WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES A
--     CONSTRAINED ACCESS TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED
--     WHEN THE VALUE OF THE OPERAND IS NOT NULL AND THE DESIGNATED
--     OBJECT HAS INDEX BOUNDS OR DISCRIMINANT VALUES THAT DO NOT EQUAL
--     THOSE SPECIFIED IN THE ACCESS TYPE'S CONSTRAINT.

-- HISTORY:
--     RJW 7/23/86
--     DWC 07/24/87  REVISED TO MAKE THE ACCESS TYPE UNCONSTRAINED
--                   AND TO PREVENT DEAD VARIABLE OPTIMIZATION.

with Report; use Report;
procedure C47009a is

begin

   Test
     ("C47009A",
      "WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION " &
      "DENOTES A CONSTRAINED ACCESS TYPE, CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN THE " &
      "VALUE OF THE OPERAND IS NOT NULL AND THE " &
      "DESIGNATED OBJECT HAS INDEX BOUNDS OR " &
      "DISCRIMINANT VALUES THAT DO NOT EQUAL THOSE " &
      "SPECIFIED IN THE ACCESS TYPE'S CONSTRAINT");

   declare

      type Arr is array (Natural range <>) of Integer;
      type Acc1 is access Arr;
      subtype Acc1s is Acc1 (Ident_Int (1) .. Ident_Int (5));
      A : Acc1;
      B : Arr (Ident_Int (2) .. Ident_Int (6));

   begin
      A := Acc1s'(new Arr'(B'First .. B'Last => 0));
      if A'First = 1 then
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC1 - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC1 - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC1");
   end;

   declare

      type Arr is array (Natural range <>, Natural range <>) of Integer;
      type Acc2 is access Arr;
      subtype Acc2s is
        Acc2 (Ident_Int (1) .. Ident_Int (5), Ident_Int (1) .. Ident_Int (1));
      A : Acc2;
      B : Arr (Ident_Int (1) .. Ident_Int (5), Ident_Int (2) .. Ident_Int (2));

   begin
      A := Acc2s'(new Arr'(B'Range => (B'Range (2) => 0)));
      if A'First = 1 then
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC2 - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC2 - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC2");
   end;

   declare

      type Rec (D : Integer) is record
         null;
      end record;

      type Acc3 is access Rec;
      subtype Acc3s is Acc3 (Ident_Int (3));
      A : Acc3;
      B : Rec (Ident_Int (5)) := (D => (Ident_Int (5)));

   begin
      A := Acc3s'(new Rec'(B));
      if A = null then
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC3 - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC3 - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC3");
   end;

   declare

      type Rec (D1, D2 : Integer) is record
         null;
      end record;

      type Acc4 is access Rec;
      subtype Acc4s is Acc4 (Ident_Int (4), Ident_Int (5));
      A : Acc4;
      B : Rec (Ident_Int (5), Ident_Int (4)) :=
        (D1 => (Ident_Int (5)), D2 => (Ident_Int (4)));

   begin
      A := Acc4s'(new Rec'(B));
      if A = null then
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC4 - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC4 - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR DISC VALUES " &
            "DIFFERENT FROM THOSE OF TYPE ACC4");
   end;

   declare

      package Pkg is
         type Rec (D : Integer) is private;

         B : constant Rec;
      private
         type Rec (D : Integer) is record
            null;
         end record;

         B : constant Rec := (D => (Ident_Int (4)));
      end Pkg;

      use Pkg;

      type Acc5 is access Rec;
      subtype Acc5s is Acc5 (Ident_Int (3));
      A : Acc5;

   begin
      A := Acc5s'(new Rec'(B));
      if A = null then
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC5 - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
            "DIFFERENT FROM THOSE OF TYPE ACC5 - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR DISC VALUES " &
            "DIFFERENT FROM THOSE OF TYPE ACC5");
   end;

   declare

      package Pkg1 is
         type Rec (D : Integer) is limited private;
         type Acc6 is access Rec;
         subtype Acc6s is Acc6 (Ident_Int (6));

         function F return Acc6;
      private
         type Rec (D : Integer) is record
            null;
         end record;
      end Pkg1;

      package body Pkg1 is

         function F return Acc6 is
         begin
            return new Rec'(D => Ident_Int (5));
         end F;

      end Pkg1;

      package Pkg2 is
      end Pkg2;

      package body Pkg2 is
         use Pkg1;

         A : Acc6;

      begin
         A := Acc6s'(F);
         if A = null then
            Failed
              ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
               "DIFFERENT FROM THOSE OF TYPE ACC6 - 1");
         else
            Failed
              ("NO EXCEPTION RAISED FOR INDEX BOUNDS " &
               "DIFFERENT FROM THOSE OF TYPE ACC6 - 2");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED FOR DISC " &
               "VALUES DIFFERENT FROM THOSE OF TYPE " & "ACC6");
      end Pkg2;

   begin
      null;
   end;

   Result;
end C47009a;
