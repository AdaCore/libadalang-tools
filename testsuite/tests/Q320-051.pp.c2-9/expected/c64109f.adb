-- C64109F.ADA

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
--   (F) CHECK THAT A FORMAL PARAMETER CAN BE USED AS AN ACTUAL IN
--       ANOTHER CALL.

-- CPP 8/20/84

with Report; use Report;
procedure C64109f is

begin
   Test
     ("C64109F",
      "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
      "RECORDS ARE PASSED CORRECTLY TO SUBPROGRAMS - " &
      "FORMAL AS AN ACTUAL");

   declare   -- (F)

      type Array_Type is array (Positive range <>) of Integer;
      subtype Array_Subtype is Array_Type (Ident_Int (1) .. Ident_Int (5));
      type Record_Type is record
         I : Integer;
         A : Array_Subtype;
      end record;
      Rec  : Record_Type := (I => 23, A => (1 .. 3 => 7, 4 .. 5 => 9));
      Bool : Boolean;

      procedure P_Called (A : in out Array_Type) is
      begin
         if A /= (7, 7, 7, 9, 9) then
            Failed ("IN OUT PARAM NOT RECEIVED CORRECTLY");
         end if;
         if A'First /= 1 or A'Last /= 5 then
            Failed ("BOUNDS WRONG - IN OUT");
         end if;
         A := (6, 6, 6, 6, 6);
      end P_Called;

      procedure P (A : in out Array_Type) is
      begin
         P_Called (A);
      end P;

      function F_Called (A : Array_Subtype) return Boolean is
         Good : Boolean;
      begin
         Good := (A = (7, 7, 7, 9, 9));
         if not Good then
            Failed ("IN PARAMETER NOT RECEIVED CORRECTLY");
         end if;
         if A'First /= 1 or A'Last /= Ident_Int (5) then
            Failed ("BOUNDS WRONG - FUNCTION");
         end if;
         return Good;
      end F_Called;

      function F (A : Array_Type) return Boolean is
      begin
         return (F_Called (A));
      end F;

      procedure P_Out_Called (A : out Array_Type) is
      begin
         if A'First /= 1 or A'Last /= 5 then
            Failed ("BOUNDS WRONG - OUT");
         end if;
         A := (8, 8, 8, 8, 8);
      end P_Out_Called;

      procedure P_Out (A : out Array_Type) is
      begin
         P_Out_Called (A);
         A := (9, 9, 9, 9, 9);
      end P_Out;

   begin     -- (F)

      P (Rec.A);
      if Rec.A /= (6, 6, 6, 6, 6) then
         Failed ("IN OUT PARAM NOT RETURNED CORRECTLY");
      end if;

      Rec.A := (7, 7, 7, 9, 9);
      Bool  := F (Rec.A);
      if not Bool then
         Failed ("IN PARAM NOT RETURNED CORRECTLY");
      end if;

      Rec.A := (7, 7, 7, 9, 9);
      P_Out (Rec.A);
      if Rec.A /= (9, 9, 9, 9, 9) then
         Failed ("OUT PARAM NOT RETURNED CORRECTLY - 2");
      end if;

   end; -- (F)

   --------------------------------------------

   Result;
end C64109f;
