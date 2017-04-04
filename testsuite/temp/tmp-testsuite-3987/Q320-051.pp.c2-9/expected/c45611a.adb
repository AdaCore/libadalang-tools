--  C45611A.ADA

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
--  CHECK THAT EXPONENTIATION OF AN INTEGER TO AN INTEGER VALUE IS
--  CORRECTLY EVALUATED.

--  H. TILTON 9/23/86

with Report; use Report;

procedure C45611a is

   I1, Int : Integer;

begin

   Test
     ("C45611A",
      "CHECK THAT EXPONENTIATION OF AN INTEGER " &
      "VALUE IS CORRECTLY EVALUATED");

   I1 := Ident_Int (0)**Ident_Int (0);

   if Ident_Int (I1) /= Ident_Int (1) then
      Failed ("INCORRECT RESULT FOR '0**0'");
   end if;

   Int := "**" (Ident_Int (0), Ident_Int (1));

   if Ident_Int (Int) /= Ident_Int (0) then
      Failed ("INCORRECT RESULT FOR '0**1'");
   end if;

   I1 := Ident_Int (6)**Ident_Int (0);

   if Ident_Int (I1) /= Ident_Int (1) then
      Failed ("INCORRECT RESULT FOR '6**0'");
   end if;

   Int := Ident_Int (156)**Ident_Int (1);

   if Ident_Int (Int) /= Ident_Int (156) then
      Failed ("INCORRECT RESULT FOR '156**1'");
   end if;

   I1 := Ident_Int (-3)**Ident_Int (0);

   if Ident_Int (I1) /= Ident_Int (1) then
      Failed ("INCORRECT RESULT FOR '(-3)**0'");
   end if;

   Int := "**" (Ident_Int (-7), Ident_Int (1));

   if Ident_Int (Int) /= Ident_Int (-7) then
      Failed ("INCORRECT RESULT FOR '(-7)**1'");
   end if;

   I1 := "**" (Ident_Int (-1), Ident_Int (2));

   if Ident_Int (I1) /= Ident_Int (1) then
      Failed ("INCORRECT RESULT FOR '(-1)**2'");
   end if;

   Int := Ident_Int (-1)**3;

   if Ident_Int (Int) /= Ident_Int (-1) then
      Failed ("INCORRECT RESULT FOR '(-1)**3'");
   end if;

   Int := "**" (Ident_Int (0), Ident_Int (2));

   if Ident_Int (Int) /= Ident_Int (0) then
      Failed ("INCORRECT RESULT FOR '0**2'");
   end if;

   Int := Ident_Int (0)**Ident_Int (10);

   if Ident_Int (Int) /= Ident_Int (0) then
      Failed ("INCORRECT RESULT FOR '0**10'");
   end if;

   Int := "**" (Ident_Int (6), Ident_Int (2));

   if Ident_Int (Int) /= Ident_Int (36) then
      Failed ("INCORRECT RESULT FOR '6**2'");
   end if;

   Int := "**" (Ident_Int (2), Ident_Int (2));

   if Ident_Int (Int) /= Ident_Int (4) then
      Failed ("INCORRECT RESULT FOR '2**2'");
   end if;

   I1 := "**" (Ident_Int (1), Ident_Int (10));

   if Ident_Int (I1) /= Ident_Int (1) then
      Failed ("INCORRECT RESULT FOR '1**10'");
   end if;

   Result;

end C45611a;
