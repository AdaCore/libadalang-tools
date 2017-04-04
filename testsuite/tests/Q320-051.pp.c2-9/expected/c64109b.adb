-- C64109B.ADA

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
--   (B) CHECK MULTIDIMENSIONAL ARRAYS.

-- CPP 8/20/84

with Report; use Report;
procedure C64109b is

begin
   Test
     ("C64109B",
      "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
      "RECORDS ARE PASSED CORRECTLY TO SUBPROGRAMS - " &
      "MULTIDIMENSIONAL ARRAYS");

   declare   -- (B)

      type Multi_Type is
        array (Positive range <>, Positive range <>) of Boolean;
      subtype Multi_Subtype is Multi_Type (1 .. 2, 1 .. 3);
      type Record_Type is record
         I : Boolean;
         A : Multi_Subtype;
      end record;
      Rec : Record_Type :=
        (I => False, A => (1 .. 2 => (1 .. 3 => Ident_Bool (True))));
      Bool : Boolean;

      procedure P1 (Arr : Multi_Type) is
      begin
         if Arr /= (1 .. 2 => (1 .. 3 => True)) then
            Failed ("IN PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= 1 or Arr'Last /= Ident_Int (2) then
            Failed ("FIRST DIM NOT CORRECT - IN PARAMETER");
         elsif Arr'First (2) /= Ident_Int (1) or Arr'Last (2) /= 3 then
            Failed ("2ND DIM NOT CORRECT - IN PARAMETER");
         end if;
      end P1;

      function F1 (Arr : Multi_Type) return Boolean is
      begin
         if Arr /= (1 .. 2 => (1 .. 3 => True)) then
            Failed ("IN PARAM NOT PASSED CORRECTLY TO FN");
         end if;

         if Arr'First /= 1 or Arr'Last /= Ident_Int (2) then
            Failed ("FIRST DIM NOT CORRECT - IN PARAMETER FN");
         elsif Arr'First (2) /= Ident_Int (1) or Arr'Last (2) /= 3 then
            Failed ("2ND DIM NOT CORRECT - IN PARAMETER FN");
         end if;
         return True;
      end F1;

      procedure P2 (Arr : in out Multi_Type) is
      begin
         if Arr /= (1 .. 2 => (1 .. 3 => True)) then
            Failed ("IN OUT PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= 1 or Arr'Last /= Ident_Int (2) then
            Failed ("FIRST DIM NOT CORRECT - IN OUT PARAMETER");
         elsif Arr'First (2) /= Ident_Int (1) or Arr'Last (2) /= 3 then
            Failed ("2ND DIM NOT CORRECT - IN OUT PARAMETER");
         end if;
         Arr := (Arr'Range (1) => (Arr'Range (2) => False));
      end P2;

      procedure P3 (Arr : out Multi_Type) is
      begin
         for I in 1 .. 2 loop
            for J in 1 .. 3 loop
               if (J mod 2) = 0 then
                  Arr (I, J) := True;
               else
                  Arr (I, J) := False;
               end if;
            end loop;
         end loop;

         if Arr'First /= 1 or Arr'Last /= Ident_Int (2) then
            Failed ("FIRST DIM NOT CORRECT - OUT PARAMETER");
         elsif Arr'First (2) /= Ident_Int (1) or Arr'Last (2) /= 3 then
            Failed ("2ND DIM NOT CORRECT - OUT PARAMETER");
         end if;
      end P3;

   begin     -- (B)

      P1 (Rec.A);
      if Rec.A /= (1 .. 2 => (1 .. 3 => True)) then
         Failed ("IN PARAM CHANGED BY PROCEDURE");
      end if;

      Bool := F1 (Rec.A);
      if Rec.A /= (1 .. 2 => (1 .. 3 => True)) then
         Failed ("IN PARAM CHANGED BY FUNCTION");
      end if;

      P2 (Rec.A);
      if Rec.A /= (1 .. 2 => (1 .. 3 => False)) then
         Failed ("IN OUT PARAM CHANGED BY PROCEDURE");
      end if;

      P3 (Rec.A);
      for I in 1 .. 2 loop
         for J in 1 .. 3 loop
            if (J mod 2) = 0 then
               if Rec.A (I, J) /= True then
                  Failed ("OUT PARAM RETURNED " & "INCORRECTLY - (B)");
               end if;
            else
               if Rec.A (I, J) /= False then
                  Failed ("OUT PARAM RETURNED " & "INCORRECTLY - (B)2");
               end if;
            end if;
         end loop;
      end loop;

   end; -- (B)

   Result;
end C64109b;
