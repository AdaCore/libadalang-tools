-- C64109E.ADA

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
--   (E) CHECK THE CASE WHERE THE FORMAL IS UNCONSTRAINED, AND ARRAYS
--       WITH DIFFERENT BOUNDS ARE PASSED AS ACTUALS.

-- CPP 8/20/84

with Report; use Report;
procedure C64109e is

begin
   Test
     ("C64109E",
      "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
      "RECORDS ARE PASSED CORRECTLY TO SUBPROGRAMS - " &
      "ARRAYS WITH DIFFERENT BOUNDS PASSED TO UNCONSTRAINED " &
      "FORMAL");

   declare   -- (E)

      subtype Subint is Integer range 0 .. 5;
      type Array_Type is array (Subint range <>) of Boolean;
      type Record_Type is record
         A : Array_Type (Ident_Int (0) .. Ident_Int (2));
         B : Array_Type (1 .. 3);
      end record;
      Rec : Record_Type :=
        (A => (0 .. 2 => Ident_Bool (True)),
         B => (1 .. 3 => Ident_Bool (False)));
      Bool : Boolean;

      procedure P1 (Arr : Array_Type; Arr2 : Array_Type) is
      begin
         if Arr /= (True, True, True) then
            Failed ("IN PARAM NOT PASSED CORRECTLY");
         end if;
         if Arr'First /= Ident_Int (0) or Arr'Last /= 2 then
            Failed ("WRONG IN PARAMETER BOUNDS - 1");
         end if;
         if Arr2 /= (False, False, False) then
            Failed ("IN PARAM NOT PASSED CORRECTLY - 2");
         end if;
         if Arr2'First /= 1 or Arr2'Last /= Ident_Int (3) then
            Failed ("WRONG IN PARAMETER BOUNDS - 2");
         end if;
      end P1;

      function F1 (Arr : Array_Type; Arr2 : Array_Type) return Boolean is
      begin
         if Arr /= (True, True, True) then
            Failed ("IN PARAM NOT PASSED CORRECTLY TO FN");
         end if;
         if Arr'First /= Ident_Int (0) or Arr'Last /= 2 then
            Failed ("WRONG IN PARAMETER BOUNDS FOR FN - 1");
         end if;
         if Arr2 /= (False, False, False) then
            Failed ("IN PARAM NOT PASSED CORRECTLY TO FN");
         end if;
         if Arr2'First /= 1 or Arr2'Last /= Ident_Int (3) then
            Failed ("WRONG IN PARAMETER BOUNDS FOR FN - 2");
         end if;
         return True;
      end F1;

      procedure P2 (Arr : in out Array_Type; Arr2 : in out Array_Type) is
      begin
         if Arr /= (True, True, True) then
            Failed ("IN OUT PARAM NOT PASSED CORRECTLY");
         end if;
         if Arr'First /= Ident_Int (0) or Arr'Last /= 2 then
            Failed ("WRONG IN OUT PARAMETER BOUNDS - 1");
         end if;
         if Arr2 /= (False, False, False) then
            Failed ("IN OUT PARAM NOT PASSED CORRECTLY");
         end if;
         if Arr2'First /= 1 or Arr2'Last /= Ident_Int (3) then
            Failed ("WRONG IN OUT PARAMETER BOUNDS - 2");
         end if;
         Arr  := (Arr'Range => False);
         Arr2 := (Arr2'Range => True);
      end P2;

      procedure P3 (Arr : out Array_Type; Arr2 : out Array_Type) is
      begin
         if Arr'First /= Ident_Int (0) or Arr'Last /= 2 then
            Failed ("WRONG OUT PARAMETER BOUNDS - 1");
         end if;
         if Arr2'First /= 1 or Arr2'Last /= Ident_Int (3) then
            Failed ("WRONG OUT  PARAMETER BOUNDS - 2");
         end if;
         Arr  := (Arr'Range => False);
         Arr2 := (Arr2'Range => True);
      end P3;

   begin     -- (E)

      P1 (Rec.A, Rec.B);
      if Rec.A /= (True, True, True) then
         Failed ("IN PARAM CHANGED BY PROCEDURE");
      end if;
      if Rec.B /= (False, False, False) then
         Failed ("IN PARAM CHANGED BY PROCEDURE - 2");
      end if;

      Bool := F1 (Rec.A, Rec.B);
      if Rec.A /= (True, True, True) then
         Failed ("IN PARAM CHANGED BY FUNCTION");
      end if;
      if Rec.B /= (False, False, False) then
         Failed ("IN PARAM CHANGED BY FUNCTION - 2");
      end if;

      P2 (Rec.A, Rec.B);
      if Rec.A /= (False, False, False) then
         Failed ("IN OUT PARAM RETURNED INCORRECTLY");
      end if;
      if Rec.B /= (True, True, True) then
         Failed ("IN OUT PARAM RETURNED INCORRECTLY - 2");
      end if;

      P3 (Rec.A, Rec.B);
      if Rec.A /= (False, False, False) then
         Failed ("OUT PARAM RETURNED INCORRECTLY");
      end if;
      if Rec.B /= (True, True, True) then
         Failed ("OUT PARAM RETURNED INCORRECTLY - 2");
      end if;

   end; -- (E)

   Result;
end C64109e;
