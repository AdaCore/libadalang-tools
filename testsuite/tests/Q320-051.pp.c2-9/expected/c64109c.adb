-- C64109C.ADA

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
-- CHECK THAT ARRAYS THAT ARE COMPONENTS OF RECORDS ARE PASSED CORRECTLY TO
-- SUBPROGRAMS. SPECIFICALLY,
--   (C) CHECK RECORDS HAVING A DISCRIMINANT, WITH MORE THAN ONE ARRAY
--       COMPONENT, WHERE THE BOUNDS OF THE ARRAY DEPEND ON THE
--       DISCRIMINANT.

-- CPP 8/20/84

with Report; use Report;
procedure C64109c is

begin
   Test
     ("C64109C",
      "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
      "RECORDS ARE PASSED CORRECTLY TO SUBPROGRAMS - " &
      "RECORDS WITH DISCRIMINANTS");

   declare   -- (C)

      subtype Subint is Integer range 1 .. 6;
      type Array_Type is array (Subint range <>) of Integer;
      type Record_Type (Bound : Integer) is record
         B  : Boolean;
         A  : Array_Type (1 .. Bound);
         Aa : Array_Type (Bound .. 6);
      end record;
      Rec : Record_Type (Bound => Ident_Int (4)) :=
        (Bound => 4,
         B     => True,
         A     => (1 .. Ident_Int (4) => 6),
         Aa    => (4 .. 6 => 8));
      Bool : Boolean;

      procedure P1 (Arr : Array_Type) is
      begin
         if Arr /= (6, 6, 6, 6) then
            Failed ("IN PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= 1 or Arr'Last /= Ident_Int (4) then
            Failed ("WRONG BOUNDS - IN PARAMETER");
         end if;
      end P1;

      function F1 (Arr : Array_Type) return Boolean is
      begin
         if Arr /= (6, 6, 6, 6) then
            Failed ("IN PARAM NOT PASSED CORRECTLY TO FN");
         end if;

         if Arr'First /= 1 or Arr'Last /= Ident_Int (4) then
            Failed ("WRONG BOUNDS - IN PARAMETER FOR FN");
         end if;
         return True;
      end F1;

      procedure P2 (Arr : in out Array_Type) is
      begin
         if Arr /= (8, 8, 8) then
            Failed ("IN OUT PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= 4 or Arr'Last /= Ident_Int (6) then
            Failed ("WRONG BOUNDS - IN OUT PARAMETER");
         end if;

         Arr := (Arr'Range => 10);
      end P2;

      procedure P3 (Arr : out Array_Type) is
      begin
         if Arr'First /= 1 or Arr'Last /= Ident_Int (4) then
            Failed ("WRONG BOUNDS - OUT PARAMETER");
         end if;
         Arr := (Arr'Range => 4);
      end P3;

   begin     -- (C)

      P1 (Rec.A);
      if Rec.A /= (6, 6, 6, 6) then
         Failed ("IN PARAM CHANGED BY PROCEDURE");
      end if;

      Bool := F1 (Rec.A);
      if Rec.A /= (6, 6, 6, 6) then
         Failed ("IN PARAM CHANGED BY FUNCTION");
      end if;

      P2 (Rec.Aa);
      if Rec.Aa /= (10, 10, 10) then
         Failed ("IN OUT PARAM NOT RETURNED CORRECTLY");
      end if;

      P3 (Rec.A);
      if Rec.A /= (4, 4, 4, 4) then
         Failed ("OUT PARAM NOT RETURNED CORRECTLY");
      end if;

   end; -- (C)

   Result;
end C64109c;
