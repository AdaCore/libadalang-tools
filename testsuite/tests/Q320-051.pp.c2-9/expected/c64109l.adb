-- C64109L.ADA

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
--       (F) CHECK THAT A FORMAL PARAMETER CAN BE USED AS AN ACTUAL IN
--       ANOTHER SUBPROGRAM CALL.

-- HISTORY:
--    TBN 07/11/86          CREATED ORIGINAL TEST.
--    JET 08/04/87          MODIFIED REC.A REFERENCES.

with Report; use Report;
procedure C64109l is

begin
   Test
     ("C64109L",
      "CHECK THAT SLICES OF ARRAYS WHICH ARE " &
      "COMPONENTS OF RECORDS ARE PASSED CORRECTLY " &
      "TO SUBPROGRAMS - FORMAL AS AN ACTUAL");

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
         if A /= (7, 7, 7) then
            Failed ("IN OUT PARAM NOT RECEIVED CORRECTLY");
         end if;
         if A'First /= 1 or A'Last /= Ident_Int (3) then
            Failed ("BOUNDS WRONG - IN OUT");
         end if;
         A := (A'Range => 6);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P_CALLED");
      end P_Called;

      procedure P (A : in out Array_Type) is
      begin
         P_Called (A);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P");
      end P;

      function F_Called (A : Array_Type) return Boolean is
         Good : Boolean;
      begin
         Good := (A = (6, 9, 9));
         if not Good then
            Failed ("IN PARAMETER NOT RECEIVED CORRECTLY");
         end if;
         if A'First /= 3 or A'Last /= Ident_Int (5) then
            Failed ("BOUNDS WRONG - FUNCTION");
         end if;
         return Good;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN FUNCTION F_CALLED");
      end F_Called;

      function F (A : Array_Type) return Boolean is
      begin
         return (F_Called (A));
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN FUNCTION F");
      end F;

      procedure P_Out_Called (A : out Array_Type) is
      begin
         if A'First /= Ident_Int (2) or A'Last /= 4 then
            Failed ("BOUNDS WRONG - OUT");
         end if;
         A := (8, 8, 8);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE " & "P_OUT_CALLED");
      end P_Out_Called;

      procedure P_Out (A : out Array_Type) is
      begin
         P_Out_Called (A);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE P_OUT");
      end P_Out;

   begin     -- (F)

      begin     -- (G)
         P (Rec.A (1 .. 3));
         if Rec.A /= (6, 6, 6, 9, 9) then
            Failed ("IN OUT PARAM NOT RETURNED CORRECTLY");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P");
      end;     -- (G)

      begin     -- (H)
         Bool := F (Rec.A (3 .. 5));
         if not Bool then
            Failed ("IN PARAM NOT RETURNED CORRECTLY");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF F");
      end;     -- (H)

      begin     -- (I)
         P_Out (Rec.A (2 .. 4));
         if Rec.A /= (6, 8, 8, 8, 9) then
            Failed ("OUT PARAM NOT RETURNED CORRECTLY - 2");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED DURING CALL OF P_OUT");
      end;     -- (I)

   end; -- (F)

   Result;
end C64109l;
