-- C45281A.ADA

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
-- CHECK THAT EQUALITY AND INEQUALITY ARE EVALUATED CORRECTLY FOR ACCESS TYPES.

-- TBN  8/8/86

with Report; use Report;
procedure C45281a is

   type Str_Name is access String;

   type Gender is (F, M);
   type Person (Sex : Gender) is record
      Name : String (1 .. 6) := "NONAME";
   end record;

   type Person_Name is access Person;
   subtype Male is Person_Name (M);
   subtype Female is Person_Name (F);

   S : Str_Name (1 .. 10) := new String'("0123456789");
   T : Str_Name (1 .. 10) := S;
   A : Male;
   B : Female;
   C : Person_Name;

begin
   Test
     ("C45281A",
      "CHECK THAT EQUALITY AND INEQUALITY ARE " &
      "EVALUATED CORRECTLY FOR ACCESS TYPES");

   if "/=" (Left => S, Right => T) then
      Failed ("INCORRECT RESULTS FOR ACCESS VALUES - 1");
   end if;
   T := new String'("0123456789");
   if "=" (S, T) then
      Failed ("INCORRECT RESULTS FOR ACCESS VALUES - 2");
   end if;

   if A /= B then
      Failed ("INCORRECT RESULTS FOR NULL ACCESS VALUES - 3");
   end if;
   if A /= C then
      Failed ("INCORRECT RESULTS FOR NULL ACCESS VALUES - 4");
   end if;

   A := new Person'(M, "THOMAS");
   if "=" (Left => A, Right => B) then
      Failed ("INCORRECT RESULTS FOR ACCESS VALUES - 5");
   end if;
   C := A;
   if C /= A then
      Failed ("INCORRECT RESULTS FOR ACCESS VALUES - 6");
   end if;
   C := new Person'(M, "THOMAS");
   if A = C then
      Failed ("INCORRECT RESULTS FOR ACCESS VALUES - 7");
   end if;

   Result;
end C45281a;
