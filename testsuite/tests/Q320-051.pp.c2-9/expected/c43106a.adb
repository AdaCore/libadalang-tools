-- C43106A.ADA

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
--     CHECK THAT BOTH NAMED AND POSITIONAL NOTATIONS ARE PERMITTED
--     WITHIN THE SAME RECORD AGGREGATE, (PROVIDED THAT ALL POSITIONAL
--     ASSOCIATIONS APPEAR BEFORE ANY NAMED ASSOCIATION).

-- HISTORY:
--     DHH 08/10/88 CREATED ORIGIANL TEST.

with Report; use Report;
procedure C43106a is

   type Rec is record
      A                   : Integer;
      B                   : Character;
      C                   : Boolean;
      D, E, F, G          : Integer;
      H, I, J, K          : Character;
      L, M, N, O          : Boolean;
      P, Q, R, S          : String (1 .. 3);
      T, U, V, W, X, Y, Z : Boolean;
   end record;
   Agg : Rec :=
     (12,
      'A',
      True,
      1,
      2,
      3,
      4,
      'B',
      'C',
      'D',
      'E',
      P | R => "ABC",
      S | Q => "DEF",
      L | X | O | U => True,
      others => False);

   function Ident_Char (X : Character) return Character is
   begin
      if Equal (3, 3) then
         return X;
      else
         return 'Z';
      end if;
   end Ident_Char;

begin
   Test
     ("C43106A",
      "CHECK THAT BOTH NAMED AND POSITIONAL NOTATIONS " &
      "ARE PERMITTED WITHIN THE SAME RECORD " &
      "AGGREGATE, (PROVIDED THAT ALL POSITIONAL " &
      "ASSOCIATIONS APPEAR BEFORE ANY NAMED " &
      "ASSOCIATION)");

   if not Ident_Bool (Agg.C) or
     not Ident_Bool (Agg.L) or
     not Ident_Bool (Agg.X) or
     not Ident_Bool (Agg.O) or
     not Ident_Bool (Agg.U) or
     Ident_Bool (Agg.M) or
     Ident_Bool (Agg.N) or
     Ident_Bool (Agg.T) or
     Ident_Bool (Agg.V) or
     Ident_Bool (Agg.W) or
     Ident_Bool (Agg.Y) or
     Ident_Bool (Agg.Z)
   then
      Failed ("BOOLEANS NOT INITIALIZED TO AGGREGATE VALUES");
   end if;

   if Ident_Str (Agg.P) /= Ident_Str (Agg.R) or
     Ident_Str (Agg.Q) /= Ident_Str (Agg.S)
   then
      Failed ("STRINGS NOT INITIALIZED CORRECTLY");
   end if;

   if Ident_Char (Agg.B) /= Ident_Char ('A') or
     Ident_Char (Agg.H) /= Ident_Char ('B') or
     Ident_Char (Agg.I) /= Ident_Char ('C') or
     Ident_Char (Agg.J) /= Ident_Char ('D') or
     Ident_Char (Agg.K) /= Ident_Char ('E')
   then
      Failed ("CHARACTERS NOT INITIALIZED CORRECTLY");
   end if;

   Result;
end C43106a;
