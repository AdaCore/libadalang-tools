-- C95090A.ADA

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
-- CHECK THAT ARRAYS THAT ARE COMPONENTS OF RECORDS ARE PASSED CORRECTLY
-- TO ENTRIES.  SPECIFICALLY,
--   (A) CHECK ALL PARAMETER MODES.

-- GLH 7/25/85

with Report; use Report;
procedure C95090a is

begin
   Test
     ("C95090A",
      "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
      "RECORDS ARE PASSED CORRECTLY TO ENTRIES");

   --------------------------------------------

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

      task T1 is
         entry E1 (Arr : Array_Type);
      end T1;

      task body T1 is
      begin
         accept E1 (Arr : Array_Type) do
            if Arr /= (7, 7, 7, 9, 9) then
               Failed ("IN PARAMETER NOT PASSED CORRECTLY");
            end if;
            if Arr'First /= Ident_Int (1) or Arr'Last /= Ident_Int (5) then
               Failed ("WRONG BOUNDS FOR IN PARAMETER");
            end if;
         end E1;
      end T1;

      task T2 is
         entry E2 (Arr : in out Array_Type);
      end T2;

      task body T2 is
      begin
         accept E2 (Arr : in out Array_Type) do
            if Arr /= (7, 7, 7, 9, 9) then
               Failed ("IN OUT PARAMETER NOT PASSED " & "CORRECTLY");
            end if;
            if Arr'First /= Ident_Int (1) or Arr'Last /= Ident_Int (5) then
               Failed ("WRONG BOUNDS FOR IN OUT PARAMETER");
            end if;
            Arr := (Arr'Range => 5);
         end E2;
      end T2;

      task T3 is
         entry E3 (Arr : out Array_Type);
      end T3;

      task body T3 is
      begin
         accept E3 (Arr : out Array_Type) do
            if Arr'First /= Ident_Int (1) or Arr'Last /= Ident_Int (5) then
               Failed ("WRONG BOUNDS FOR OUT PARAMETER");
            end if;
            Arr := (Arr'Range => 3);
         end E3;
      end T3;

   begin     -- (A)

      T1.E1 (Rec.A);
      if Rec.A /= (7, 7, 7, 9, 9) then
         Failed ("IN PARAM CHANGED BY PROCEDURE");
      end if;

      T2.E2 (Rec.A);
      if Rec.A /= (5, 5, 5, 5, 5) then
         Failed ("IN OUT PARAM RETURNED INCORRECTLY");
      end if;

      T3.E3 (Rec.A);
      if Rec.A /= (3, 3, 3, 3, 3) then
         Failed ("OUT PARAM RETURNED INCORRECTLY");
      end if;

   end; -- (A)

   --------------------------------------------

   Result;
end C95090a;
