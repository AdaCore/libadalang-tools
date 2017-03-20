-- C36205G.ADA

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
-- CHECK THAT ATTRIBUTES GIVE THE CORRECT VALUES FOR
-- UNCONSTRAINED FORMAL PARAMETERS.

-- ATTRIBUTES OF DYNAMIC NON-NULL AGGREGATES

-- DAT 2/17/81
-- JBG 9/11/81
-- JWC 6/28/85   RENAMED TO -AB

with Report;
procedure C36205g is

   use Report;

   type I_A is array (Integer range <>) of Integer;
   type I_A_2 is array (Integer range <>, Integer range <>) of Integer;
   A10   : I_A (1 .. 10);
   A20   : I_A (18 .. 20);
   I10   : Integer := Ident_Int (10);
   A2_10 : I_A_2 (1 .. I10, 3 + I10 .. I10 * I10);       -- 1..10, 13..20
   A2_20 : I_A_2 (11 .. 3 * I10, I10 + 11 .. I10 + I10);   -- 11..30, 21..20
   subtype Str is String;
   Alf : constant Str (Ident_Int (1) .. Ident_Int (5)) := "ABCDE";
   Arf : Str (5 .. 9)                                  := Alf;

   procedure P1 (A : I_A; Fir, Las : Integer; S : String) is
   begin
      if A'First /= Fir or A'First (1) /= Fir then
         Failed ("'FIRST IS WRONG " & S);
      end if;

      if A'Last /= Las or A'Last (1) /= Las then
         Failed ("'LAST IS WRONG " & S);
      end if;

      if A'Length /= Las - Fir + 1 or A'Length /= A'Length (1) then
         Failed ("'LENGTH IS WRONG " & S);
      end if;

      if (Las not in A'Range and Las >= Fir) or
        (Fir not in A'Range and Las >= Fir) or
        Fir - 1 in A'Range or
        Las + 1 in A'Range (1)
      then
         Failed ("'RANGE IS WRONG " & S);
      end if;

   end P1;

   procedure P2 (A : I_A_2; F1, L1, F2, L2 : Integer; S : String) is
   begin
      if A'First /= A'First (1) or A'First /= F1 then
         Failed ("'FIRST(1) IS WRONG " & S);
      end if;

      if A'Last (1) /= L1 then
         Failed ("'LAST(1) IS WRONG " & S);
      end if;

      if A'Length (1) /= A'Length or A'Length /= L1 - F1 + 1 then
         Failed ("'LENGTH(1) IS WRONG " & S);
      end if;

      if F1 - 1 in A'Range or
        (F1 not in A'Range and F1 <= L1) or
        (L1 not in A'Range (1) and F1 <= L1) or
        L1 + 1 in A'Range (1)
      then
         Failed ("'RANGE(1) IS WRONG " & S);
      end if;

      if A'First (2) /= F2 then
         Failed ("'FIRST(2) IS WRONG " & S);
      end if;

      if A'Last (2) /= L2 then
         Failed ("'LAST(2) IS WRONG " & S);
      end if;

      if L2 - F2 /= A'Length (2) - 1 then
         Failed ("'LENGTH(2) IS WRONG " & S);
      end if;

      if F2 - 1 in A'Range (2) or
        (F2 not in A'Range (2) and A'Length (2) > 0) or
        (L2 not in A'Range (2) and A'Length (2) /= 0) or
        L2 + 1 in A'Range (2)
      then
         Failed ("'RANGE(2) IS WRONG " & S);
      end if;
   end P2;

   procedure S1 (S : Str; F, L : Integer; Mess : String) is
   begin
      if S'First /= F then
         Failed ("STRING 'FIRST IS WRONG " & Mess);
      end if;

      if S'Last (1) /= L then
         Failed ("STRING 'LAST IS WRONG " & Mess);
      end if;

      if S'Length /= L - F + 1 or S'Length (1) /= S'Length then
         Failed ("STRING 'LENGTH IS WRONG " & Mess);
      end if;

      if
        (F <= L and
         (F not in S'Range or
          L not in S'Range or
          F not in S'Range (1) or
          L not in S'Range (1))) or
        F - 1 in S'Range or
        L + 1 in S'Range (1)
      then
         Failed ("STRING 'RANGE IS WRONG " & Mess);
      end if;
   end S1;

begin
   Test
     ("C36205G",
      "CHECKING ATTRIBUTE VALUES POSSESSED BY FORMAL " &
      "PARAMETERS WHOSE ACTUALS ARE UNCONSTRAINED " &
      "ARRAYS - DYNAMIC NON-NULL AGGREGATES");

   P1 ((Ident_Int (3) .. Ident_Int (5) => 2), 3, 5, "P1 16");
   P1 ((Ident_Int (5) .. 5 => 5), 5, 5, "P1 17");

   Result;
end C36205g;
