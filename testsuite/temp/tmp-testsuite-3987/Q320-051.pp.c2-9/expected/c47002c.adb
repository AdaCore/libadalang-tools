-- C47002C.ADA

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
-- CHECK THAT VALUES BELONGING TO EACH CLASS OF TYPE CAN BE WRITTEN AS
-- THE OPERANDS OF QUALIFIED EXPRESSIONS.
-- THIS TEST IS FOR ARRAY, RECORD, AND ACCESS TYPES.

-- RJW 7/23/86

with Report; use Report;
procedure C47002c is

begin

   Test
     ("C47002C",
      "CHECK THAT VALUES HAVING ARRAY, RECORD, AND " &
      "ACCESS TYPES CAN BE WRITTEN AS THE OPERANDS " &
      "OF QUALIFIED EXPRESSIONS");

   declare -- ARRAY TYPES.

      type Arr is array (Positive range <>) of Integer;
      subtype Arr1 is Arr (1 .. 1);
      subtype Arr5 is Arr (1 .. 5);

      type Narr is new Arr;
      subtype Narr2 is Narr (2 .. 2);

      type Tarr is array (Natural range <>, Natural range <>) of Integer;
      subtype Tarr15 is Tarr (1 .. 1, 1 .. 5);
      subtype Tarr51 is Tarr (1 .. 5, 1 .. 1);

      type Ntarr is new Tarr;
      subtype Ntarr26 is Ntarr (2 .. 6, 2 .. 6);

      function F (X : Arr) return Arr is
      begin
         return X;
      end F;

      function F (X : Narr) return Narr is
      begin
         return X;
      end F;

      function F (X : Tarr) return Tarr is
      begin
         return X;
      end F;

      function F (X : Ntarr) return Ntarr is
      begin
         return X;
      end F;

   begin
      if F (Arr1'(others => 0))'Last /= 1 then
         Failed ("INCORRECT RESULTS FOR SUBTYPE ARR1");
      end if;

      if F (Arr5'(others => 0))'Last /= 5 then
         Failed ("INCORRECT RESULTS FOR SUBTYPE ARR5");
      end if;

      if F (Narr2'(others => 0))'First /= 2 or
        F (Narr2'(others => 0))'Last /= 2
      then
         Failed ("INCORRECT RESULTS FOR SUBTYPE NARR2");
      end if;

      if F (Tarr15'(others => (others => 0)))'Last /= 1 or
        F (Tarr15'(others => (others => 0)))'Last (2) /= 5
      then
         Failed ("INCORRECT RESULTS FOR SUBTYPE TARR15");
      end if;

      if F (Tarr51'(others => (others => 0)))'Last /= 5 or
        F (Tarr51'(others => (others => 0)))'Last (2) /= 1
      then
         Failed ("INCORRECT RESULTS FOR SUBTYPE TARR51");
      end if;

      if F (Ntarr26'(others => (others => 0)))'First /= 2 or
        F (Ntarr26'(others => (others => 0)))'Last /= 6 or
        F (Ntarr26'(others => (others => 0)))'First (2) /= 2 or
        F (Ntarr26'(others => (others => 0)))'Last (2) /= 6
      then
         Failed ("INCORRECT RESULTS FOR SUBTYPE NTARR26");
      end if;

   end;

   declare -- RECORD TYPES.

      type Gender is (Male, Female, Neuter);

      type Man is record
         Age : Positive;
      end record;

      type Woman is record
         Age : Positive;
      end record;

      type Android is new Man;

      function F (X : Woman) return Gender is
      begin
         return Female;
      end F;

      function F (X : Man) return Gender is
      begin
         return Male;
      end F;

      function F (X : Android) return Gender is
      begin
         return Neuter;
      end F;

   begin
      if F (Man'(Age => 23)) /= Male then
         Failed ("INCORRECT RESULTS FOR SUBTYPE MAN");
      end if;

      if F (Woman'(Age => 38)) /= Female then
         Failed ("INCORRECT RESULTS FOR SUBTYPE WOMAN");
      end if;

      if F (Android'(Age => 2_001)) /= Neuter then
         Failed ("INCORRECT RESULTS FOR TYPE ANDRIOD");
      end if;
   end;

   declare -- ACCESS TYPES.

      type Code is (Old, Brandnew, Wreck);

      type Car (D : Code) is record
         null;
      end record;

      type Key is access Car;

      type Key_Old is access Car (Old);
      Ko : Key_Old := new Car'(D => Old);

      type Key_Wreck is access Car (Wreck);

      type Key_Card is new Key;
      Kc : Key_Card := new Car'(D => Brandnew);

      function F (X : Key_Old) return Code is
      begin
         return Old;
      end F;

      function F (X : Key_Wreck) return Code is
      begin
         return Wreck;
      end F;

      function F (X : Key_Card) return Code is
      begin
         return Brandnew;
      end F;
   begin
      if Key_Old'(Ko) /= Ko then
         Failed ("INCORRECT RESULTS FOR TYPE KEY_OLD - 1");
      end if;

      if Key_Card'(Kc) /= Kc then
         Failed ("INCORRECT RESULTS FOR TYPE KEY_CARD - 1");
      end if;

      if F (Key_Old'(null)) /= Old then
         Failed ("INCORRECT RESULTS FOR SUBTYPE KEY_OLD - 2");
      end if;

      if F (Key_Wreck'(null)) /= Wreck then
         Failed ("INCORRECT RESULTS FOR SUBTYPE KEY_WRECK");
      end if;

      if F (Key_Card'(null)) /= Brandnew then
         Failed ("INCORRECT RESULTS FOR TYPE KEY_CARD - 2");
      end if;
   end;

   Result;
end C47002c;
