-- C74210A.ADA

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
-- CHECK THAT OPERATOR SYMBOLS OVERLOADED IN A PACKAGE ARE
--   USED AND DERIVED IN PREFERENCE TO THOSE OF THE PARENT OF A DERIVED
--   PRIVATE TYPE.

-- CHECK THAT OPERATOR DEFINITIONS FOR A PRIVATE TYPE MAY BE
--   OVERLOADED OUTSIDE THE PACKAGE.

-- CHECK THAT EQUALITY CAN BE DEFINED FOR LIMITED TYPES AND COMPOSITE
--   TYPES WITH LIMITED COMPONENTS.

-- DAT 5/11/81

with Report; use Report;

procedure C74210a is
begin
   Test ("C74210A", "OVERLOADED OPERATORS FOR PRIVATE TYPES");

   declare
      package P is
         type T is private;
         function "+" (X, Y : T) return T;
         One, Two : constant T;

         type L is limited private;
         type A is array (0 .. 0) of L;
         type R is record
            C : L;
         end record;
         function "=" (X, Y : L) return Boolean;
      private
         type T is new Integer;
         One : constant T := T (Ident_Int (1));
         Two : constant T := T (Ident_Int (2));
         type L is (Enum);
      end P;
      use P;

      Vr : R;
      Va : A;

      package body P is
         function "+" (X, Y : T) return T is
         begin
            return 1;
         end "+";

         function "=" (X, Y : L) return Boolean is
         begin
            return Ident_Bool (False);
         end "=";
      begin
         Vr := (C => Enum);
         Va := (0 => Vr.C);
      end P;
   begin
      if One + Two /= One then
         Failed ("WRONG ""+"" OPERATOR");
      end if;

      declare
         type New_T is new T;

         function "=" (X, Y : A) return Boolean;
         function "=" (X, Y : R) return Boolean;

         function "+" (X, Y : T) return T is
         begin
            return Two;
         end "+";

         function "=" (X, Y : A) return Boolean is
         begin
            return X (0) = Y (0);
         end "=";

         function "=" (X, Y : R) return Boolean is
         begin
            return X.C = Y.C;
         end "=";
      begin
         if One + Two /= Two then
            Failed ("WRONG DERIVED ""+"" OPERATOR");
         end if;

         if Vr = Vr or Va = Va then
            Failed ("CANNOT OVERLOAD ""="" CORRECTLY");
         end if;
      end;
   end;

   Result;
end C74210a;
