-- C64109G.ADA

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
-- CHECK THAT SLICES OF ARRAYS ARE PASSED CORRECTLY TO SUBPROGRAMS.
-- SPECIFICALLY,
--   (A) CHECK ALL PARAMETER MODES.

-- CPP 8/28/84
-- PWN 05/31/96 Corrected spelling problem.

with Report; use Report;
procedure C64109g is

begin
   Test
     ("C64109G",
      "CHECK THAT SLICES OF ARRAYS ARE PASSED " & "CORRECTLY TO SUBPROGRAMS");

   --------------------------------------------

   declare   -- (A)

      subtype Subint is Integer range 1 .. 5;
      type Array_Type is array (Subint range <>) of Integer;
      Arr  : Array_Type (1 .. 5) := (1 .. 3 => 7, 4 .. 5 => 9);
      Bool : Boolean;

      procedure P1 (S : Array_Type) is
      begin
         if S (Ident_Int (3)) /= 7 then
            Failed ("IN PARAMETER NOT PASSED CORRECTLY - (A)");
         end if;
         if S (4) /= 9 then
            Failed ("IN PARAMETER NOT PASSED CORRECTLY - (A)2");
         end if;
      end P1;

      function F1 (S : Array_Type) return Boolean is
      begin
         if S (3) /= 7 then
            Failed ("IN PARAMETER NOT PASSED CORRECTLY - (A)");
         end if;
         if S (Ident_Int (4)) /= 9 then
            Failed ("IN PARAMETER NOT PASSED CORRECTLY - (A)2");
         end if;
         return True;
      end F1;

      procedure P2 (S : in out Array_Type) is
      begin
         if S (3) /= 7 then
            Failed ("IN OUT PARAM NOT PASSED CORRECTLY - (A)");
         end if;
         if S (4) /= 9 then
            Failed ("IN OUT PARAM NOT PASSED CORRECTLY - (A)2");
         end if;
         for I in 3 .. 4 loop
            S (I) := 5;
         end loop;
      end P2;

      procedure P3 (S : out Array_Type) is
      begin
         for I in 3 .. 4 loop
            S (I) := 3;
         end loop;
      end P3;

   begin     -- (A)

      P1 (Arr (3 .. 4));
      if Arr (3) /= 7 then
         Failed ("IN PARAM CHANGED BY PROCEDURE - (A)");
      end if;
      if Arr (4) /= 9 then
         Failed ("IN PARAM CHANGED BY PROCEDURE - (A)2");
      end if;

      Bool := F1 (Arr (Ident_Int (3) .. Ident_Int (4)));
      if Arr (3) /= 7 then
         Failed ("IN PARAM CHANGED BY FUNCTION - (A)");
      end if;
      if Arr (4) /= 9 then
         Failed ("IN PARAM CHANGED BY FUNCTION - (A)2");
      end if;

      P2 (Arr (3 .. 4));
      for I in 3 .. 4 loop
         if Arr (I) /= 5 then
            Failed ("IN OUT PARAM RETURNED INCORRECTLY - (A)");
         end if;
      end loop;

      P3 (Arr (Ident_Int (3) .. 4));
      for I in 3 .. 4 loop
         if Arr (I) /= 3 then
            Failed ("OUT PARAM RETURNED INCORRECTLY - (A)");
         end if;
      end loop;

   end;

   Result;

end C64109g;
