-- C43204E.ADA

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
--     CHECK THAT AN ARRAY AGGREGATE WITH AN OTHERS CHOICE CAN APPEAR
--     AS THE INITIALIZATION EXPRESSION OF A CONSTRAINED CONSTANT,
--     VARIABLE OBJECT DECLARATION, OR RECORD COMPONENT DECLARATION,
--     AND THAT THE BOUNDS OF THE AGGREGATE ARE DETERMINED CORRECTLY.

-- HISTORY:
--     JET 08/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43204e is

   type Arr11 is array (Integer range -3 .. 3) of Integer;
   type Arr12 is array (Ident_Int (-3) .. Ident_Int (3)) of Integer;
   type Arr13 is array (Ident_Int (1) .. Ident_Int (-1)) of Integer;
   type Arr21 is
     array (Integer range -1 .. 1, Integer range -1 .. 1) of Integer;
   type Arr22 is
     array
       (Ident_Int (-1) .. Ident_Int (1),
        Ident_Int (-1) .. Ident_Int (1)) of Integer;
   type Arr23 is
     array (Integer range -1 .. 1, Ident_Int (-1) .. Ident_Int (1)) of Integer;
   type Arr24 is
     array
       (Ident_Int (1) .. Ident_Int (-1),
        Ident_Int (-1) .. Ident_Int (1)) of Integer;

   Ca11 : constant Arr11 := (1, others => Ident_Int (2));
   Ca12 : constant Arr12 := (others => Ident_Int (2));
   Ca13 : constant Arr13 := (others => Ident_Int (2));
   Ca21 : constant Arr21 := (others => (-1 .. 1 => Ident_Int (2)));
   Ca22 : constant Arr22 := (others => (-1 .. 1 => Ident_Int (2)));
   Ca23 : constant Arr23 := (-1 .. 1 => (others => Ident_Int (2)));
   Ca24 : constant Arr24 := (others => (others => Ident_Int (2)));

   Va11 : Arr11 := (1, 1, others => Ident_Int (2));
   Va12 : Arr12 := (others => Ident_Int (2));
   Va13 : Arr13 := (others => Ident_Int (2));
   Va21 : Arr21 := ((1, 1, 1), others => (-1 .. 1 => Ident_Int (2)));
   Va22 : Arr22 := (-1 => (1, 1, 1), 0 .. 1 => (others => Ident_Int (2)));
   Va23 : Arr23 := (others => (others => Ident_Int (2)));
   Va24 : Arr24 := (others => (others => Ident_Int (2)));

   type Rec is record
      Ra11 : Arr11 := (1, 1, 1, others => Ident_Int (2));
      Ra12 : Arr12 := (others => Ident_Int (2));
      Ra13 : Arr13 := (others => Ident_Int (2));
      Ra21 : Arr21 :=
        ((1, 1, 1),
         (1, 1, 1),
         others => (Ident_Int (2), Ident_Int (2), Ident_Int (2)));
      Ra22 : Arr22 := (others => (others => Ident_Int (2)));
      Ra23 : Arr23 :=
        (-1 => (others => 1), 0 .. 1 => (others => Ident_Int (2)));
      Ra24 : Arr24 := (others => (others => Ident_Int (2)));
   end record;

   R : Rec;

begin
   Test
     ("C43204E",
      "CHECK THAT AN ARRAY AGGREGATE WITH AN OTHERS " &
      "CHOICE CAN APPEAR AS THE INITIALIZATION " &
      "EXPRESSION OF A CONSTRAINED CONSTANT, " &
      "VARIABLE OBJECT DECLARATION, OR RECORD " &
      "COMPONENT DECLARATION, AND THAT THE BOUNDS OF " &
      "THE AGGREGATE ARE DETERMINED CORRECTLY");

   if Ca11 /= (1, 2, 2, 2, 2, 2, 2) then
      Failed ("INCORRECT VALUE OF CA11");
   end if;

   if Ca12 /= (2, 2, 2, 2, 2, 2, 2) then
      Failed ("INCORRECT VALUE OF CA12");
   end if;

   if Ca13'Length /= 0 then
      Failed ("INCORRECT VALUE OF CA13");
   end if;

   if Ca21 /= ((2, 2, 2), (2, 2, 2), (2, 2, 2)) then
      Failed ("INCORRECT VALUE OF CA21");
   end if;

   if Ca22 /= ((2, 2, 2), (2, 2, 2), (2, 2, 2)) then
      Failed ("INCORRECT VALUE OF CA22");
   end if;

   if Ca23 /= ((2, 2, 2), (2, 2, 2), (2, 2, 2)) then
      Failed ("INCORRECT VALUE OF CA23");
   end if;

   if Ca24'Length /= 0 or Ca24'Length (2) /= 3 then
      Failed ("INCORRECT VALUE OF CA24");
   end if;

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

   if R.Ra11 /= (1, 1, 1, 2, 2, 2, 2) then
      Failed ("INCORRECT VALUE OF RA11");
   end if;

   if R.Ra12 /= (2, 2, 2, 2, 2, 2, 2) then
      Failed ("INCORRECT VALUE OF RA12");
   end if;

   if R.Ra13'Length /= 0 then
      Failed ("INCORRECT VALUE OF RA13");
   end if;

   if R.Ra21 /= ((1, 1, 1), (1, 1, 1), (2, 2, 2)) then
      Failed ("INCORRECT VALUE OF RA21");
   end if;

   if R.Ra22 /= ((2, 2, 2), (2, 2, 2), (2, 2, 2)) then
      Failed ("INCORRECT VALUE OF RA22");
   end if;

   if R.Ra23 /= ((1, 1, 1), (2, 2, 2), (2, 2, 2)) then
      Failed ("INCORRECT VALUE OF RA23");
   end if;

   if R.Ra24'Length /= 0 or R.Ra24'Length (2) /= 3 then
      Failed ("INCORRECT VALUE OF RA24");
   end if;

   Result;

exception
   when others =>
      Failed ("UNEXPECTED CONSTRAINT_ERROR OR OTHER EXCEPTION " & "RAISED");

      Result;
end C43204e;
