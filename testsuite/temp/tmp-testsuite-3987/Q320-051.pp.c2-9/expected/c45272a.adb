-- C45272A.ADA

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
-- CHECK THAT EQUALITY AND INEQUALITY ARE EVALUATED CORRECTLY FOR
-- RECORDS WHOSE COMPONENTS HAVE CHANGEABLE DISCRIMINANTS, INCLUDING
-- RECORDS DESIGNATED BY ACCESS VALUES.

-- TBN  8/7/86

with Report; use Report;
procedure C45272a is

   subtype Int is Integer range 0 .. 20;
   type Varstr (Len : Int := 0) is record
      Val : String (1 .. Len);
   end record;
   type Varrec is record
      A, B : Varstr;
   end record;

   type Cell2;
   type Link is access Cell2;
   type Cell1 (Nam_Len : Int := 0) is record
      Name : String (1 .. Nam_Len);
   end record;
   type Cell2 is record
      One      : Cell1;
      Two      : Cell1;
      New_Link : Link;
   end record;

   X, Y  : Varrec;
   Front : Link := new Cell2'((5, "XXYZZ"), (5, "YYYZZ"), null);
   Back  : Link := new Cell2'((5, "XXYZZ"), (5, "YYYZZ"), null);

begin
   Test
     ("C45272A",
      "CHECK THAT EQUALITY AND INEQUALITY ARE " &
      "EVALUATED CORRECTLY FOR RECORDS WHOSE " &
      "COMPONENTS HAVE CHANGEABLE DISCRIMINANTS");

   X := ((5, "AAAXX"), (5, "BBBYY"));
   Y := ((5, "AAAZZ"), (5, "BBBYY"));
   if X = Y then
      Failed ("INCORRECT RESULTS FOR RECORDS - 1");
   end if;

   X.A := (3, "HHH");
   Y.A := (Ident_Int (3), Ident_Str ("HHH"));
   if X /= Y then
      Failed ("INCORRECT RESULTS FOR RECORDS - 2");
   end if;

   if Front.all /= Back.all then
      Failed ("INCORRECT RESULTS FOR RECORDS - 3");
   end if;

   Back.New_Link := Front;
   if Front.all = Back.all then
      Failed ("INCORRECT RESULTS FOR RECORDS - 4");
   end if;

   Front.New_Link := Front;
   if Front.all /= Back.all then
      Failed ("INCORRECT RESULTS FOR RECORDS - 5");
   end if;

   Front.One := (5, "XXXXX");
   Back.One  := (5, "ZZZZZ");
   if Front.all = Back.all then
      Failed ("INCORRECT RESULTS FOR RECORDS - 6");
   end if;

   Front.One := (3, "XXX");
   Back.One  := (3, "XXX");
   if Front.all /= Back.all then
      Failed ("INCORRECT RESULTS FOR RECORDS - 7");
   end if;

   Result;
end C45272a;
