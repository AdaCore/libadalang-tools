-- C64109H.ADA

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
--       (A) CHECK ALL PARAMETER MODES.

-- HISTORY:
--    TBN 07/11/86          CREATED ORIGINAL TEST.
--    JET 08/04/87          MODIFIED REC.A REFERENCES.

with Report; use Report;
procedure C64109h is

begin
   Test
     ("C64109H",
      "CHECK THAT SLICES OF ARRAYS WHICH ARE " &
      "COMPONENTS OF RECORDS ARE PASSED CORRECTLY " & "TO SUBPROGRAMS");

   declare   -- (A)

      type Array_Type is array (Positive range <>) of Integer;
      subtype Array_Subtype is Array_Type (1 .. Ident_Int (5));
      type Record_Type is record
         I : Integer;
         A : Array_Subtype;
      end record;
      Rec : Record_Type :=
        (I => 23, A => (1 .. 3 => Ident_Int (7), 4 .. 5 => 9));
      Bool : Boolean;

      procedure P1 (Arr : Array_Type) is
      begin
         if Arr /= (7, 9, 9) then
            Failed ("IN PARAMETER NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= Ident_Int (3) or Arr'Last /= Ident_Int (5) then
            Failed ("WRONG BOUNDS FOR IN PARAMETER");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P1");
      end P1;

      function F1 (Arr : Array_Type) return Boolean is
      begin
         if Arr /= (7, 7, 9) then
            Failed ("IN PARAMETER NOT PASSED CORRECTLY TO FN");
         end if;
         if Arr'First /= Ident_Int (2) or Arr'Last /= Ident_Int (4) then
            Failed ("WRONG BOUNDS FOR IN PARAMETER FOR FN");
         end if;

         return True;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN FUNCTION F1");
      end F1;

      procedure P2 (Arr : in out Array_Type) is
      begin
         if Arr /= (7, 7, 7, 9) then
            Failed ("IN OUT PARAMETER NOT PASSED " & "CORRECTLY");
         end if;
         if Arr'First /= Ident_Int (1) or Arr'Last /= Ident_Int (4) then
            Failed ("WRONG BOUNDS FOR IN OUT PARAMETER");
         end if;
         Arr := (Arr'Range => 5);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P2");
      end P2;

      procedure P3 (Arr : out Array_Type) is
      begin
         if Arr'First /= Ident_Int (3) or Arr'Last /= Ident_Int (4) then
            Failed ("WRONG BOUNDS FOR OUT PARAMETER");
         end if;

         Arr := (Arr'Range => 3);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P3");
      end P3;

   begin     -- (A)

      begin     -- (B)
         P1 (Rec.A (3 .. 5));
         if Rec.A /= (7, 7, 7, 9, 9) then
            Failed ("IN PARAM CHANGED BY PROCEDURE");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P1");
      end;     -- (B)

      begin     -- (C)
         Bool := F1 (Rec.A (2 .. 4));
         if Rec.A /= (7, 7, 7, 9, 9) then
            Failed ("IN PARAM CHANGED BY FUNCTION");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF F1");
      end;     -- (C)

      begin     -- (D)
         P2 (Rec.A (1 .. 4));
         if Rec.A /= (5, 5, 5, 5, 9) then
            Failed ("IN OUT PARAM RETURNED INCORRECTLY");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P2");
      end;     -- (D)

      begin     -- (E)
         P3 (Rec.A (3 .. 4));
         if Rec.A /= (5, 5, 3, 3, 9) then
            Failed ("OUT PARAM RETURNED INCORRECTLY");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P3");
      end;     -- (E)

   end; -- (A)

   Result;
end C64109h;
