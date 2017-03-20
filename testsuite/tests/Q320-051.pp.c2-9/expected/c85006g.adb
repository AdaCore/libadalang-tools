-- C85006G.ADA

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
--     CHECK THAT ANY SUBTYPE CONSTRAINT IMPOSED BY THE TYPE MARK USED
--     IN THE SLICE RENAMING DECLARATION IS IGNORED, AND THAT THE
--     SUBTYPE CONSTRAINT ASSOCIATED WITH THE RENAMED VARIABLE IS
--     USED INSTEAD.

-- HISTORY:
--     JET 07/26/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85006g is

   subtype Str is String (1 .. 10);

   S : String (1 .. 30) := Ident_Str ("IT WAS A DARK AND STORMY NIGHT");
   T : Str              := Ident_Str ("0123456789");

   Dg1 : String (1 .. 30) := Ident_Str ("IT WAS A DARK AND STORMY NIGHT");
   Dg2 : Str              := Ident_Str ("0123456789");

   Xs : Str renames S (10 .. 24);
   Xt : String renames T (1 .. 5);

   generic
      G1 : in out Str;
      G2 : in out String;
   package Gen is
      Xg1 : Str renames G1 (10 .. 24);
      Xg2 : String renames G2 (1 .. 5);
   end Gen;

   package Pack is new Gen (Dg1, Dg2);
   use Pack;

begin
   Test
     ("C85006G",
      "CHECK THAT ANY SUBTYPE CONSTRAINT IMPOSED BY " &
      "THE TYPE MARK USED IN THE SLICE RENAMING " &
      "DECLARATION IS IGNORED, AND THAT THE SUBTYPE " &
      "CONSTRAINT ASSOCIATED WITH THE RENAMED " &
      "VARIABLE IS USED INSTEAD");

   if Xs'First /= Ident_Int (10) or
     Xs'Last /= Ident_Int (24) or
     Xs'Length /= Ident_Int (15)
   then
      Failed ("INCORRECT VALUE OF SLICE ATTRIBUTES - 1");
   end if;

   if Xs /= "DARK AND STORMY" then
      Failed ("INCORRECT VALUE OF RENAMING SLICE - 1");
   end if;

   Xs := Ident_Str ("STORMY AND DARK");

   if S /= "IT WAS A STORMY AND DARK NIGHT" then
      Failed ("INCORRECT VALUE OF ORIGINAL STRING - 1");
   end if;

   if Xt'First /= Ident_Int (1) or
     Xt'Last /= Ident_Int (5) or
     Xt'Length /= Ident_Int (5)
   then
      Failed ("INCORRECT VALUE OF SLICE ATTRIBUTES - 2");
   end if;

   if Xt /= "01234" then
      Failed ("INCORRECT VALUE OF RENAMING SLICE - 2");
   end if;

   Xt := Ident_Str ("43210");

   if T /= "4321056789" then
      Failed ("INCORRECT VALUE OF ORIGINAL STRING - 2");
   end if;

   if Xg1'First /= Ident_Int (10) or
     Xg1'Last /= Ident_Int (24) or
     Xg1'Length /= Ident_Int (15)
   then
      Failed ("INCORRECT VALUE OF SLICE ATTRIBUTES - G1");
   end if;

   if Xg1 /= "DARK AND STORMY" then
      Failed ("INCORRECT VALUE OF RENAMING SLICE - G1");
   end if;

   Xg1 := Ident_Str ("STORMY AND DARK");

   if Dg1 /= "IT WAS A STORMY AND DARK NIGHT" then
      Failed ("INCORRECT VALUE OF ORIGINAL STRING - G1");
   end if;

   if Xg2'First /= Ident_Int (1) or
     Xg2'Last /= Ident_Int (5) or
     Xg2'Length /= Ident_Int (5)
   then
      Failed ("INCORRECT VALUE OF SLICE ATTRIBUTES - G2");
   end if;

   if Xg2 /= "01234" then
      Failed ("INCORRECT VALUE OF RENAMING SLICE - G2");
   end if;

   Xg2 := Ident_Str ("43210");

   if Dg2 /= "4321056789" then
      Failed ("INCORRECT VALUE OF ORIGINAL STRING - G2");
   end if;

   Result;

exception
   when others =>
      Failed ("UNEXPECTED EXCEPTION RAISED");
      Result;
end C85006g;
