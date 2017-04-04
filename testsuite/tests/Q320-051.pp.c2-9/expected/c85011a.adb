-- C85011A.ADA

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
--     CHECK THAT A PACKAGE CAN BE RENAMED AND THE NEW NAME CAN APPEAR
--     IN A RENAMING DECLARATION, AND THAT A 'USE' CLAUSE CAN REFER TO
--     THE PACKAGE BY EITHER NAME, INCLUDING RENAMINGS OF GENERIC AND
--     NONGENERIC PACKAGES INSIDE THEMSELVES.

-- HISTORY:
--     JET 04/28/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85011a is

   package Pack1 is
      I : Natural := 0;
      package Packa renames Pack1;
   end Pack1;

   generic
      type T is range <>;
   package Gpack is
      J : T := T'First;
      package Packb renames Gpack;
   end Gpack;

   package Pack2 is new Gpack (Natural);

   package Pack3 renames Pack1;
   package Pack4 renames Pack2;
   package Pack5 renames Pack3;
   package Pack6 renames Pack4;

begin
   Test
     ("C85011A",
      "CHECK THAT A PACKAGE CAN BE RENAMED AND THE " &
      "NEW NAME CAN APPEAR IN A RENAMING " &
      "DECLARATION, AND THAT A 'USE' CLAUSE CAN " &
      "REFER TO THE PACKAGE BY EITHER NAME, " &
      "INCLUDING RENAMINGS OF GENERIC AND NONGENERIC " &
      "PACKAGES INSIDE THEMSELVES");

   if Pack1.I /= Ident_Int (0) then
      Failed ("INCORRECT VALUE OF PACK1.I");
   end if;

   if Pack2.J /= Ident_Int (0) then
      Failed ("INCORRECT VALUE OF PACK2.J");
   end if;

   if Pack3.I /= Ident_Int (0) then
      Failed ("INCORRECT VALUE OF PACK3.I");
   end if;

   if Pack4.J /= Ident_Int (0) then
      Failed ("INCORRECT VALUE OF PACK4.J");
   end if;

   if Pack5.I /= Ident_Int (0) then
      Failed ("INCORRECT VALUE OF PACK5.I");
   end if;

   if Pack6.J /= Ident_Int (0) then
      Failed ("INCORRECT VALUE OF PACK6.J");
   end if;

   if Pack1.Packa.I /= Ident_Int (0) then
      Failed ("INCORRECT VALUE OF PACK1.PACKA.I");
   end if;

   if Pack2.Packb.J /= Ident_Int (0) then
      Failed ("INCORRECT VALUE OF PACK2.PACKB.J");
   end if;

   declare
      use Pack1, Pack2;
   begin
      if I /= Ident_Int (0) then
         Failed ("INCORRECT VALUE OF I (1)");
      end if;

      if J /= Ident_Int (0) then
         Failed ("INCORRECT VALUE OF J (1)");
      end if;
   end;

   declare
      use Pack3, Pack4;
   begin
      if I /= Ident_Int (0) then
         Failed ("INCORRECT VALUE OF I (2)");
      end if;

      if J /= Ident_Int (0) then
         Failed ("INCORRECT VALUE OF J (2)");
      end if;
   end;

   declare
      use Pack5, Pack6;
   begin
      if I /= Ident_Int (0) then
         Failed ("INCORRECT VALUE OF I (3)");
      end if;

      if J /= Ident_Int (0) then
         Failed ("INCORRECT VALUE OF J (3)");
      end if;
   end;

   declare
      use Pack1.Packa, Pack2.Packb;
   begin
      if I /= Ident_Int (0) then
         Failed ("INCORRECT VALUE OF I (4)");
      end if;

      if J /= Ident_Int (0) then
         Failed ("INCORRECT VALUE OF J (4)");
      end if;
   end;

   Result;
end C85011a;
