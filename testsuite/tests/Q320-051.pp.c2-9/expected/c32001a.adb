-- C32001A.ADA

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
-- CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR SCALAR TYPES, THE SUBTYPE
-- INDICATION AND THE INITIALIZATION EXPRESSIONS ARE EVALUATED ONCE FOR EACH
-- NAMED OBJECT THAT IS DECLARED AND THE SUBTYPE INDICATION IS EVALUATED FIRST.
-- ALSO, CHECK THAT THE EVALUATIONS YIELD THE SAME RESULT AS A SEQUENCE OF
-- SINGLE OBJECT DECLARATIONS.

-- RJW 7/16/86

with Report; use Report;

procedure C32001a is

   Bump : array (1 .. 8) of Integer := (others => 0);

   function F (I : Integer) return Integer is
   begin
      Bump (I) := Bump (I) + 1;
      return Bump (I);
   end F;

begin
   Test
     ("C32001A",
      "CHECK THAT IN MULTIPLE OBJECT DECLARATION " &
      "FOR SCALAR TYPES, THE SUBTYPE INDICATION " &
      "AND THE INITIALIZATION EXPRESSIONS ARE " &
      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
      "EVALUATIONS YIELD THE SAME RESULT AS A " &
      "SEQUENCE OF SINGLE OBJECT DECLARATIONS");

   declare

      type Day is (Mon, Tues, Wed, Thurs, Fri);
      D1, D2   : Day range Mon .. Day'Val (F (1)) := Day'Val (F (1) - 1);
      Cd1, Cd2 : constant Day range Mon .. Day'Val (F (2)) :=
        Day'Val (F (2) - 1);

      I1, I2   : Integer range 0 .. F (3)          := F (3) - 1;
      Ci1, Ci2 : constant Integer range 0 .. F (4) := F (4) - 1;

      type Flt is digits 3 range -5.0 .. 5.0;
      Fl1, Fl2   : Flt range 0.0 .. Flt (F (5))          := Flt (F (5) - 1);
      Cfl1, Cfl2 : constant Flt range 0.0 .. Flt (F (6)) := Flt (F (6) - 1);

      type Fix is delta 1.0 range -5.0 .. 5.0;
      Fi1, Fi2   : Fix range 0.0 .. Fix (F (7))          := Fix (F (7) - 1);
      Cfi1, Cfi2 : constant Fix range 0.0 .. Fix (F (8)) := Fix (F (8) - 1);

   begin
      if D1 /= Tues then
         Failed ("D1 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if D2 /= Thurs then
         Failed ("D2 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Cd1 /= Tues then
         Failed ("CD1 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Cd2 /= Thurs then
         Failed ("CD2 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if I1 /= 1 then
         Failed ("I1 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if I2 /= 3 then
         Failed ("I2 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Ci1 /= 1 then
         Failed ("CI1 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Ci2 /= 3 then
         Failed ("CI2 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Fl1 /= 1.0 then
         Failed ("FL1 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Fl2 /= 3.0 then
         Failed ("FL2 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Cfl1 /= 1.0 then
         Failed ("CFL1 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Cfl2 /= 3.0 then
         Failed ("CFL2 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Fi1 /= 1.0 then
         Failed ("FI1 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Fi2 /= 3.0 then
         Failed ("FI2 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Cfi1 /= 1.0 then
         Failed ("CFI1 NOT INITIALIZED TO CORRECT VALUE");
      end if;

      if Cfi2 /= 3.0 then
         Failed ("CFI2 NOT INITIALIZED TO CORRECT VALUE");
      end if;

   end;

   Result;
end C32001a;
