-- C52010A.ADA

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
-- CHECK THAT RECORD ASSIGNMENTS USE "COPY" SEMANTICS. (PART I).

-- FACTORS AFFECTING THE SITUATION TO BE TESTED:
--
--        COMPONENT TYPE                     * INTEGER
--                                           * BOOLEAN (OMITTED)
--                                           * CHARACTER (OMITTED)
--                                           * USER-DEFINED ENUMERATION
--
--        DERIVED VS. NON-DERIVED
--
--        TYPE VS. SUBTYPE
--
--        ORDER OF COMPONENT ASSIGNMENTS     * LEFT-TO-RIGHT
--                                           * RIGHT-TO-LEFT
--                                           * INSIDE-OUT
--                                           * OUTSIDE IN

-- RM 02/23/80
-- SPS 3/21/83

with Report;
procedure C52010a is

   use Report;

   type Enum is
     (Aa, Bb, Cc, Dd, Ee, Ff, Gg, Hh, Ii, Jj, Kk, Ll, Mm, Nn, Pp, Qq, Tt, Uu,
      Vv, Ww, Xx, Yy);

begin

   Test
     ("C52010A", "CHECK THAT RECORD ASSIGNMENTS USE ""COPY""" & " SEMANTICS");

   declare
      type Rec is record
         X, Y : Integer;
      end record;
      R : Rec;
   begin

      R := (5, 8);
      R := (X => 1, Y => R.X);
      if R /= (1, 5) then
         Failed ("WRONG VALUE  (1)");
      end if;

      R := (5, 8);
      R := (Y => 1, X => R.Y);
      if R /= (8, 1) then
         Failed ("WRONG VALUE  (2)");
      end if;

      R := (5, 8);
      R := (R.Y + 1, R.X + 1);
      if R /= (9, 6) then
         Failed ("WRONG VALUE  (3)");
      end if;

   end;

   declare
      type Rec3 is record
         Deep0 : Integer;
         Deep  : Integer;
      end record;
      type Rec2 is record
         Yx       : Rec3;
         Moderate : Integer;
      end record;
      type Rec is record
         Shallow : Integer;
         Yz      : Rec2;
      end record;
      R : Rec;
   begin
      R := (0, ((5, 1), 2));
      R := (R.Yz.Moderate + 8, ((7, R.Shallow + 1), R.Yz.Yx.Deep + 99));
      if R /= (10, ((7, 1), 100)) then
         Failed ("WRONG VALUE  (4)");
      end if;
   end;

   declare
      type Sub_Enum is new Enum range Aa .. Dd;
      type Rec is record
         X, Y : Sub_Enum;
      end record;
      R : Rec;
   begin
      R := (Aa, Cc);
      R := (X => Bb, Y => R.X);
      if R /= (Bb, Aa) then
         Failed ("WRONG VALUE  (5)");
      end if;

      R := (Aa, Cc);
      R := (Y => Bb, X => R.Y);
      if R /= (Cc, Bb) then
         Failed ("WRONG VALUE  (6)");
      end if;

      R := (Aa, Cc);
      R := (Sub_Enum'Succ (R.Y), Sub_Enum'Succ (R.X));
      if R /= (Dd, Bb) then
         Failed ("WRONG VALUE  (7)");
      end if;

   end;

   declare
      type Rec3 is record
         Deep0 : Enum;
         Deep  : Enum;
      end record;
      type Rec2 is record
         Yx       : Rec3;
         Moderate : Enum;
      end record;
      type Rec is record
         Shallow : Enum;
         Yz      : Rec2;
      end record;
      R : Rec;
   begin

      R := (Tt, ((Yy, Ii), Aa));

      R :=
        (Enum'Succ (Enum'Succ (R.Yz.Moderate)),
         ((Aa, Enum'Succ (R.Shallow)),
          (Enum'Succ (Enum'Succ (Enum'Succ (Enum'Succ (R.Yz.Yx.Deep)))))));

      if R /= (Cc, ((Aa, Uu), Mm)) then
         Failed ("WRONG VALUE  (8)");
      end if;

   end;

   Result;

end C52010a;
