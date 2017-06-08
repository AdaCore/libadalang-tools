-- C43204G.ADA

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
--     CONSTRAINED FORMAL PARAMETER OF AN ENTRY, AND THAT THE BOUNDS
--     OF THE AGGREGATE ARE DETERMINED CORRECTLY.

-- HISTORY:
--     JET 08/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43204g is

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

   task T is
      entry E
        (Ea11 : Arr11 := (1, 1, 1, 1, others => Ident_Int (2));
         Ea12 : Arr12 := (others => Ident_Int (2));
         Ea13 : Arr13 := (others => Ident_Int (2));
         Ea21 : Arr21 :=
           ((1, 1, 1),
            (1, 1, 1),
            (1, 1, 1),
            others => (-1 .. 1 => Ident_Int (2)));
         Ea22 : Arr22 := ((others => Ident_Int (2)), (1, 1, 1), (1, 1, 1));
         Ea23 : Arr23 :=
           (-1 .. 0 => (others => 1), 1 => (others => Ident_Int (2)));
         Ea24 : Arr24 := (others => (others => Ident_Int (2))));
   end T;

   task body T is
   begin
      accept E
        (Ea11 : Arr11 := (1, 1, 1, 1, others => Ident_Int (2));
         Ea12 : Arr12 := (others => Ident_Int (2));
         Ea13 : Arr13 := (others => Ident_Int (2));
         Ea21 : Arr21 :=
           ((1, 1, 1),
            (1, 1, 1),
            (1, 1, 1),
            others => (-1 .. 1 => Ident_Int (2)));
         Ea22 : Arr22 := ((others => Ident_Int (2)), (1, 1, 1), (1, 1, 1));
         Ea23 : Arr23 :=
           (-1 .. 0 => (others => 1), 1 => (others => Ident_Int (2)));
         Ea24 : Arr24 := (others => (others => Ident_Int (2))))
      do
         if Ea11 /= (1, 1, 1, 1, 2, 2, 2) then
            Failed ("INCORRECT VALUE OF EA11");
         end if;

         if Ea12 /= (2, 2, 2, 2, 2, 2, 2) then
            Failed ("INCORRECT VALUE OF EA12");
         end if;

         if Ea13'Length /= 0 then
            Failed ("INCORRECT VALUE OF EA13");
         end if;

         if Ea21 /= ((1, 1, 1), (1, 1, 1), (1, 1, 1)) then
            Failed ("INCORRECT VALUE OF EA21");
         end if;

         if Ea22 /= ((2, 2, 2), (1, 1, 1), (1, 1, 1)) then
            Failed ("INCORRECT VALUE OF EA22");
         end if;

         if Ea23 /= ((1, 1, 1), (1, 1, 1), (2, 2, 2)) then
            Failed ("INCORRECT VALUE OF EA23");
         end if;

         if Ea24'Length /= 0 or Ea24'Length (2) /= 3 then
            Failed ("INCORRECT VALUE OF EA24");
         end if;
      end E;
   end T;

begin
   Test
     ("C43204G",
      "CHECK THAT AN AGGREGATE WITH AN OTHERS CLAUSE " &
      "CAN APPEAR AS A CONSTRAINED FORMAL PARAMETER " &
      "OF AN ENTRY, AND THAT THE BOUNDS OF THE " &
      "AGGREGATE ARE DETERMINED CORRECTLY");

   T.E;

   Result;

exception
   when others =>
      Failed ("UNEXPECTED CONSTRAINT_ERROR OR OTHER EXCEPTION " & "RAISED");

      if T'Callable then
         T.E;
      end if;

      Result;
end C43204g;
