-- C43204H.ADA

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
--     CHECK THAT AN AGGREGATE WITH AN OTHERS CLAUSE CAN APPEAR AS A
--     CONSTRAINED FORMAL PARAMETER OF A GENERIC UNIT, AND THAT THE
--     BOUNDS OF THE AGGREGATE ARE DETERMINED CORRECTLY.

-- HISTORY:
--     JET 08/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43204h is

   type Arr11 is array (Integer range -3 .. 3) of Integer;
   type Arr12 is array (Ident_Int (-3) .. Ident_Int (3)) of Integer;
   type Arr13 is array (Ident_Int (1) .. Ident_Int (-1)) of Integer;
   type Arr21 is
     array (Integer range -1 .. 1, Integer range -1 .. 1) of Integer;
   type Arr22 is
     array
       (Ident_Int (-1) ..
            Ident_Int (1),
          Ident_Int (-1) ..
            Ident_Int (1)) of Integer;
   type Arr23 is
     array (Integer range -1 .. 1, Ident_Int (-1) .. Ident_Int (1)) of Integer;
   type Arr24 is
     array
       (Ident_Int (1) ..
            Ident_Int (-1),
          Ident_Int (-1) ..
            Ident_Int (1)) of Integer;

   generic
      Ga11 : Arr11 := (1, 1, 1, 1, 1, others => Ident_Int (2));
      Ga12 : Arr12 := (others => Ident_Int (2));
      Ga13 : Arr13 := (others => Ident_Int (2));
      Ga21 : Arr21 := ((1, 1, 1), (1, 1, 1), (others => Ident_Int (2)));
      Ga22 : Arr22 := ((1, 1, 1), (others => Ident_Int (2)), (1, 1, 1));
      Ga23 : Arr23 := ((1, 1, 1), (others => Ident_Int (2)), (1, 1, 1));
      Ga24 : Arr24 := (others => (others => Ident_Int (2)));
   procedure Gen;

   procedure Gen is
   begin
      if Ga11 /= (1, 1, 1, 1, 1, 2, 2) then
         Failed ("INCORRECT VALUE OF GA11");
      end if;

      if Ga12 /= (2, 2, 2, 2, 2, 2, 2) then
         Failed ("INCORRECT VALUE OF GA12");
      end if;

      if Ga13'Length /= 0 then
         Failed ("INCORRECT VALUE OF GA13");
      end if;

      if Ga21 /= ((1, 1, 1), (1, 1, 1), (2, 2, 2)) then
         Failed ("INCORRECT VALUE OF GA21");
      end if;

      if Ga22 /= ((1, 1, 1), (2, 2, 2), (1, 1, 1)) then
         Failed ("INCORRECT VALUE OF GA22");
      end if;

      if Ga23 /= ((1, 1, 1), (2, 2, 2), (1, 1, 1)) then
         Failed ("INCORRECT VALUE OF GA23");
      end if;

      if Ga24'Length /= 0 or Ga24'Length (2) /= 3 then
         Failed ("INCORRECT VALUE OF GA24");
      end if;
   end Gen;

   procedure Procg is new Gen;

begin
   Test
     ("C43204H",
      "CHECK THAT AN AGGREGATE WITH AN OTHERS CLAUSE " &
      "CAN APPEAR AS A CONSTRAINED FORMAL PARAMETER " &
      "OF A GENERIC UNIT, AND THAT THE BOUNDS OF " &
      "THE AGGREGATE ARE DETERMINED CORRECTLY");

   Procg;

   Result;

exception
   when others =>
      Failed ("UNEXPECTED CONSTRAINT_ERROR OR OTHER EXCEPTION " & "RAISED");

      Result;
end C43204h;
