-- C64109J.ADA

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
--      (D) CHECK OBJECTS DESIGNATED BY ACCESS TYPES.

-- HISTORY:
--    TBN 07/10/86          CREATED ORIGINAL TEST.
--    JET 08/04/87          MODIFIED PTR.A REFERENCES.

with Report; use Report;
procedure C64109j is

begin
   Test
     ("C64109J",
      "CHECK THAT SLICES OF ARRAYS WHICH ARE " &
      "COMPONENTS OF RECORDS ARE PASSED CORRECTLY " &
      "TO SUBPROGRAMS - OBJECTS DESIGNATED BY ACCESS " &
      "TYPES");

   declare   -- (D)

      subtype Index is Integer range 1 .. 5;
      type Array_Type is array (Index range <>) of Integer;
      subtype Array_Subtype is Array_Type (1 .. Ident_Int (5));
      type Node_Type;
      type Access_Type is access Node_Type;
      type Node_Type is record
         A    : Array_Subtype;
         Next : Access_Type;
      end record;
      Ptr : Access_Type :=
        new Node_Type'
          (A => (Ident_Int (1) .. 5 => Ident_Int (5)), Next => null);
      Bool : Boolean;

      procedure P1 (Arr : Array_Type) is
      begin
         if Arr /= (5, 5, 5) then
            Failed ("IN PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= Ident_Int (1) or Arr'Last /= 3 then
            Failed ("WRONG BOUNDS - IN PARAMETER");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P1");
      end P1;

      function F1 (Arr : Array_Type) return Boolean is
      begin
         if Arr /= (5, 5, 5) then
            Failed ("IN PARAM NOT PASSED CORRECTLY TO FN");
         end if;

         if Arr'First /= Ident_Int (2) or Arr'Last /= 4 then
            Failed ("WRONG BOUNDS - IN PARAMETER FOR FN");
         end if;

         return True;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN FUNCTION F1");
      end F1;

      procedure P2 (Arr : in out Array_Type) is
      begin
         if Arr /= (5, 5, 5) then
            Failed ("IN OUT PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= Ident_Int (1) or Arr'Last /= 3 then
            Failed ("WRONG BOUNDS - IN OUT PARAMETER");
         end if;

         Arr := (Arr'Range => 6);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P2");
      end P2;

      procedure P3 (Arr : out Array_Type) is
      begin

         if Arr'First /= Ident_Int (3) or Arr'Last /= 5 then
            Failed ("WRONG BOUNDS - OUT PARAMETER");
         end if;

         Arr := (Arr'Range => 7);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P3");
      end P3;

   begin     -- (D)

      begin     -- (E)
         P1 (Ptr.A (1 .. 3));
         if Ptr.A /= (5, 5, 5, 5, 5) then
            Failed ("IN PARAM CHANGED BY PROCEDURE");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P1");
      end;     -- (E)

      begin     -- (F)
         Bool := F1 (Ptr.A (2 .. 4));
         if Ptr.A /= (5, 5, 5, 5, 5) then
            Failed ("IN PARAM CHANGED BY FUNCTION");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF F1");
      end;     -- (F)

      begin     -- (G)
         P2 (Ptr.A (1 .. 3));
         if Ptr.A /= (6, 6, 6, 5, 5) then
            Failed ("IN OUT PARAM NOT RETURNED CORRECTLY");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P2");
      end;     -- (G)

      begin     -- (H)
         P3 (Ptr.A (3 .. 5));
         if Ptr.A /= (6, 6, 7, 7, 7) then
            Failed ("OUT PARAM NOT RETURNED CORRECTLY");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P3");
      end;     -- (H)

   end; -- (D)

   Result;
end C64109j;
