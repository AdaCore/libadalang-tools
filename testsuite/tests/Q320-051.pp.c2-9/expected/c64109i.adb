-- C64109I.ADA

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
--    CHECK THAT SLICES OF ARRAYS WHICH ARE COMPONENTS OF RECORDS ARE
--    PASSED CORRECTLY TO SUBPROGRAMS.  SPECIFICALLY,
--    (C) CHECK RECORDS HAVING A DISCRIMINANT, WITH MORE THAN ONE ARRAY
--       COMPONENT, WHERE THE BOUNDS OF THE ARRAY DEPEND ON THE
--       DISCRIMINANT.

-- HISTORY:
--    TBN 07/10/86          CREATED ORIGINAL TEST.
--    JET 08/04/87          REMOVED PARTIAL ARRAY REFERENCES IN
--                          RECORD FIELDS.

with Report; use Report;
procedure C64109i is

begin
   Test
     ("C64109I",
      "CHECK THAT SLICES OF ARRAYS WHICH ARE " &
      "COMPONENTS OF RECORDS ARE PASSED CORRECTLY " &
      "TO SUBPROGRAMS - RECORDS WITH DISCRIMINANTS");

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
         if Arr /= (6, 6, 6) then
            Failed ("IN PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= 1 or Arr'Last /= Ident_Int (3) then
            Failed ("WRONG BOUNDS - IN PARAMETER");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P1");
      end P1;

      function F1 (Arr : Array_Type) return Boolean is
      begin
         if Arr /= (6, 6, 6) then
            Failed ("IN PARAM NOT PASSED CORRECTLY TO FN");
         end if;

         if Arr'First /= 2 or Arr'Last /= Ident_Int (4) then
            Failed ("WRONG BOUNDS - IN PARAMETER FOR FN");
         end if;
         return True;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN FUNCTION F1");
      end F1;

      procedure P2 (Arr : in out Array_Type) is
      begin
         if Arr /= (8, 8) then
            Failed ("IN OUT PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= 4 or Arr'Last /= Ident_Int (5) then
            Failed ("WRONG BOUNDS - IN OUT PARAMETER");
         end if;

         Arr := (Arr'Range => 10);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P2");
      end P2;

      procedure P3 (Arr : out Array_Type) is
      begin
         if Arr'First /= 2 or Arr'Last /= Ident_Int (3) then
            Failed ("WRONG BOUNDS - OUT PARAMETER");
         end if;
         Arr := (Arr'Range => 4);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P3");
      end P3;

   begin     -- (C)

      begin     -- (D)
         P1 (Rec.A (1 .. 3));
         if Rec.A /= (6, 6, 6, 6) then
            Failed ("IN PARAM CHANGED BY PROCEDURE");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P1");
      end;     -- (D)

      begin     -- (E)
         Bool := F1 (Rec.A (2 .. 4));
         if Rec.A /= (6, 6, 6, 6) then
            Failed ("IN PARAM CHANGED BY FUNCTION");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF F1");
      end;     -- (E)

      begin     -- (F)
         P2 (Rec.Aa (4 .. 5));
         if Rec.Aa /= (10, 10, 8) then
            Failed ("IN OUT PARAM NOT RETURNED CORRECTLY");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P2");
      end;     -- (F)

      begin     -- (G)
         P3 (Rec.A (2 .. 3));
         if Rec.A /= (6, 4, 4, 6) then
            Failed ("OUT PARAM NOT RETURNED CORRECTLY");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P3");
      end;     -- (G)

   end; -- (C)

   Result;
end C64109i;
