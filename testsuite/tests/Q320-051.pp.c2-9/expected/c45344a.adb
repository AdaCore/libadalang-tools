-- C45344A.ADA

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
-- CHECK THAT THE CORRECT RESULT IS PRODUCED WHEN A FUNCTION RETURNS THE RESULT
-- OF A CATENATION WHOSE BOUNDS ARE NOT DEFINED STATICALLY.

-- R.WILLIAMS 9/1/86

with Report; use Report;
procedure C45344a is

begin
   Test
     ("C45344A",
      "CHECK THAT THE CORRECT RESULT IS PRODUCED " &
      "WHEN A FUNCTION RETURNS THE RESULT OF A " &
      "CATENATION WHOSE BOUNDS ARE NOT DEFINED " & "STATICALLY");

   declare
      subtype Int is Integer range Ident_Int (1) .. Ident_Int (30);

      type Arr is array (Int range <>) of Integer;
      subtype Carr is Arr (1 .. 9);
      C : Carr;

      Ar1 : Arr (Ident_Int (2) .. Ident_Int (4)) :=
        (Ident_Int (2) .. Ident_Int (4) => 1);

      Ar2 : Arr (Ident_Int (6) .. Ident_Int (6)) :=
        (Ident_Int (6) .. Ident_Int (6) => 2);

      Ar3 : Arr (Ident_Int (4) .. Ident_Int (2));

      function F (A, B : Arr; N : Natural) return Arr is
      begin
         if N = 0 then
            return A & B;
         else
            return F (A & B, B, N - 1);
         end if;
      end F;

      function G (A : Integer; B : Arr; N : Natural) return Arr is
      begin
         if N = 0 then
            return A & B;
         else
            return G (A, A & B, N - 1);
         end if;
      end G;

      function H (A : Arr; B : Integer; N : Natural) return Arr is
      begin
         if N = 0 then
            return A & B;
         else
            return H (A & B, B, N - 1);
         end if;
      end H;

      procedure Check (X, Y : Arr; F, L : Integer; Str : String) is
         Ok : Boolean := True;
      begin
         if X'First /= F and X'Last /= L then
            Failed ("INCORRECT RANGE FOR " & Str);
         else
            for I in F .. L loop
               if X (I) /= Y (I) then
                  Ok := False;
               end if;
            end loop;

            if not Ok then
               Failed ("INCORRECT VALUE FOR " & Str);
            end if;
         end if;
      end Check;

   begin
      C := (1 .. 4 => 1, 5 .. 9 => 2);
      Check (F (Ar1, Ar2, Ident_Int (3)), C, 2, 8, "F - 1");
      Check (F (Ar3, Ar2, Ident_Int (3)), C, 6, 9, "F - 2");
      Check (F (Ar2, Ar3, Ident_Int (3)), C, 6, 6, "F - 3");

      C := (1 .. 4 => 5, 5 .. 9 => 1);
      Check (G (5, Ar1, Ident_Int (3)), C, 1, 7, "G - 1");
      Check (G (5, Ar3, Ident_Int (3)), C, 1, 4, "G - 2");

      Check (H (Ar3, 5, Ident_Int (3)), C, 1, 4, "H - 1");

      C := (1 .. 4 => 1, 5 .. 9 => 5);
      Check (H (Ar1, 5, Ident_Int (3)), C, 2, 8, "H - 2");
   end;

   Result;
end C45344a;
