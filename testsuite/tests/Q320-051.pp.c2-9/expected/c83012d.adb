-- C83012D.ADA

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
--     CHECK THAT WITHIN A GENERIC PACKAGE INSTANTIATION, A DECLARATION
--     HAVING THE SAME IDENTIFIER AS THE PACKAGE IS VISIBLE BY
--     SELECTION.

-- HISTORY:
--     JET 08/11/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C83012d is

   package Pack is
      subtype Pack1 is Integer;
      Pack2 : Integer := 2;
   end Pack;

   type Rec is record
      Pack3 : Integer;
      Pack4 : Integer;
   end record;

   R : Rec := (Pack3 => 3, Pack4 => 1);

   generic
      type T is range <>;
   package Gen1 is
      J : Integer := Ident_Int (1);
   end Gen1;

   generic
      I : Integer;
   package Gen2 is
      J : Integer := Ident_Int (I);
   end Gen2;

   generic
      R : Rec;
   package Gen3 is
      J : Integer := Ident_Int (R.Pack4);
   end Gen3;

   generic
      Pack6 : Integer;
   package Gen4 is
      J : Integer := Ident_Int (Pack6);
   end Gen4;

   function Func (Pack5 : Integer) return Integer is
   begin
      return Ident_Int (Pack5);
   end Func;

   package Pack1 is new Gen1 (Pack.Pack1);
   package Pack2 is new Gen2 (Pack.Pack2);
   package Pack3 is new Gen2 (R.Pack3);
   package Pack4 is new Gen3 ((1, Pack4 => 4));
   package Pack5 is new Gen2 (Func (Pack5 => 5));
   package Pack6 is new Gen4 (Pack6 => 6);

begin
   Test
     ("C83012D",
      "CHECK THAT WITHIN A GENERIC PACKAGE " &
      "INSTANTIATION, A DECLARATION HAVING THE SAME " &
      "IDENTIFIER AS THE PACKAGE IS VISIBLE BY " &
      "SELECTION");

   if Pack1.J /= 1 then
      Failed ("INCORRECT VALUE OF PACK1.J");
   end if;

   if Pack2.J /= 2 then
      Failed ("INCORRECT VALUE OF PACK2.J");
   end if;

   if Pack3.J /= 3 then
      Failed ("INCORRECT VALUE OF PACK3.J");
   end if;

   if Pack4.J /= 4 then
      Failed ("INCORRECT VALUE OF PACK4.J");
   end if;

   if Pack5.J /= 5 then
      Failed ("INCORRECT VALUE OF PACK5.J");
   end if;

   if Pack6.J /= 6 then
      Failed ("INCORRECT VALUE OF PACK6.J");
   end if;

   Result;

end C83012d;
