-- C64109D.ADA

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
-- TO SUBPROGRAMS.  SPECIFICALLY,
--   (D) CHECK OBJECTS DESIGNATED BY ACCESS TYPES.

-- CPP 8/20/84

with Report; use Report;
procedure C64109d is

begin
   Test
     ("C64109D",
      "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
      "RECORDS ARE PASSED CORRECTLY TO SUBPROGRAMS - " &
      "OBJECTS DESIGNATED BY ACCESS TYPES");

   declare   -- (D)

      subtype Index is Integer range 1 .. 3;
      type Array_Type is array (Index range <>) of Integer;
      subtype Array_Subtype is Array_Type (1 .. Ident_Int (3));
      type Node_Type;
      type Access_Type is access Node_Type;
      type Node_Type is record
         A    : Array_Subtype;
         Next : Access_Type;
      end record;
      Ptr : Access_Type :=
        new Node_Type'
          (A => (Ident_Int (1) .. 3 => Ident_Int (5)), Next => null);
      Bool : Boolean;

      procedure P1 (Arr : Array_Type) is
      begin
         if Arr /= (5, 5, 5) then
            Failed ("IN PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= Ident_Int (1) or Arr'Last /= 3 then
            Failed ("WRONG BOUNDS - IN PARAMETER");
         end if;
      end P1;

      function F1 (Arr : Array_Type) return Boolean is
      begin
         if Arr /= (5, 5, 5) then
            Failed ("IN PARAM NOT PASSED CORRECTLY TO FN");
         end if;

         if Arr'First /= Ident_Int (1) or Arr'Last /= 3 then
            Failed ("WRONG BOUNDS - IN PARAMETER FOR FN");
         end if;

         return True;
      end F1;

      procedure P2 (Arr : in out Array_Subtype) is
      begin
         if Arr /= (5, 5, 5) then
            Failed ("IN OUT PARAM NOT PASSED CORRECTLY");
         end if;

         if Arr'First /= Ident_Int (1) or Arr'Last /= 3 then
            Failed ("WRONG BOUNDS - IN OUT PARAMETER");
         end if;

         Arr := (others => 6);
      end P2;

      procedure P3 (Arr : out Array_Type) is
      begin

         if Arr'First /= Ident_Int (1) or Arr'Last /= 3 then
            Failed ("WRONG BOUNDS - OUT PARAMETER");
         end if;

         Arr := (Arr'Range => 7);
      end P3;

   begin     -- (D)

      P1 (Ptr.A);
      if Ptr.A /= (5, 5, 5) then
         Failed ("IN PARAM CHANGED BY PROCEDURE");
      end if;

      Bool := F1 (Ptr.A);
      if Ptr.A /= (5, 5, 5) then
         Failed ("IN PARAM CHANGED BY FUNCTION");
      end if;

      P2 (Ptr.A);
      if Ptr.A /= (6, 6, 6) then
         Failed ("IN OUT PARAM NOT RETURNED CORRECTLY");
      end if;

      P3 (Ptr.A);
      if Ptr.A /= (7, 7, 7) then
         Failed ("OUT PARAM NOT RETURNED CORRECTLY");
      end if;

   end; -- (D)

   Result;
end C64109d;
