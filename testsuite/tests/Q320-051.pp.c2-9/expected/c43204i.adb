-- C43204I.ADA

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
--     CHECK THAT AN AGGREGATE WITH AN OTHERS CLAUSE CAN APPEAR AS THE
--     EXPRESSION IN AN ASSIGNMENT STATEMENT, AND THAT THE BOUNDS OF
--     THE AGGREGATE ARE DETERMINED CORRECTLY.

-- HISTORY:
--     JET 08/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43204i is

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

   Va11 : Arr11;
   Va12 : Arr12;
   Va13 : Arr13;
   Va21 : Arr21;
   Va22 : Arr22;
   Va23 : Arr23;
   Va24 : Arr24;

begin
   Test
     ("C43204I",
      "CHECK THAT AN AGGREGATE WITH AN OTHERS CLAUSE " &
      "CAN APPEAR AS THE EXPRESSION IN AN ASSIGNMENT " &
      "STATEMENT, AND THAT THE BOUNDS OF THE " &
      "AGGREGATE ARE DETERMINED CORRECTLY");

   Va11 := (1, 1, others => Ident_Int (2));
   Va12 := (others => Ident_Int (2));
   Va13 := (others => Ident_Int (2));
   Va21 := ((1, 1, 1), others => (-1 .. 1 => Ident_Int (2)));
   Va22 := (-1 => (1, 1, 1), 0 .. 1 => (others => Ident_Int (2)));
   Va23 := (others => (others => Ident_Int (2)));
   Va24 := (others => (others => Ident_Int (2)));

   if Va11 /= (1, 1, 2, 2, 2, 2, 2) then
      Failed ("INCORRECT VALUE OF VA11");
   end if;

   if Va12 /= (2, 2, 2, 2, 2, 2, 2) then
      Failed ("INCORRECT VALUE OF VA12");
   end if;

   if Va13'Length /= 0 then
      Failed ("INCORRECT VALUE OF VA13");
   end if;

   if Va21 /= ((1, 1, 1), (2, 2, 2), (2, 2, 2)) then
      Failed ("INCORRECT VALUE OF VA21");
   end if;

   if Va22 /= ((1, 1, 1), (2, 2, 2), (2, 2, 2)) then
      Failed ("INCORRECT VALUE OF VA22");
   end if;

   if Va23 /= ((2, 2, 2), (2, 2, 2), (2, 2, 2)) then
      Failed ("INCORRECT VALUE OF VA23");
   end if;

   if Va24'Length /= 0 or Va24'Length (2) /= 3 then
      Failed ("INCORRECT VALUE OF VA24");
   end if;

   Result;

exception
   when others =>
      Failed ("UNEXPECTED CONSTRAINT_ERROR OR OTHER EXCEPTION " & "RAISED");

      Result;
end C43204i;
