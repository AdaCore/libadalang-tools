-- C45271A.ADA

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
-- CHECK THAT EQUALITY AND INEQUALITY ARE EVALUATED CORRECTLY FOR RECORDS WHOSE
-- COMPONENTS DO NOT HAVE CHANGEABLE DISCRIMINANTS.

-- TBN  8/6/86

with Report; use Report;
procedure C45271a is

   subtype Int is Integer range 1 .. 20;
   type Array_Bool is array (1 .. 5) of Boolean;

   type Rec_Type1 is record
      Bool : Array_Bool;
      A    : Integer;
   end record;

   type Rec_Type2 (Len : Int := 3) is record
      A : String (1 .. Len);
   end record;

   type Rec_Type3 (Num : Int := 1) is record
      A : Rec_Type1;
   end record;

   Rec1, Rec2 : Rec_Type1     := (A => 2, others => (others => True));
   Rec3, Rec4 : Rec_Type2 (5) := (5, "WHERE");
   Rec5, Rec6 : Rec_Type2;
   Rec7, Rec8 : Rec_Type3;
   Rec9,
   Rec10 : Rec_Type3 (3) :=
     (Num => 3, A => (A => 5, Bool => (others => False)));

begin
   Test
     ("C45271A",
      "CHECK THAT EQUALITY AND INEQUALITY ARE " &
      "EVALUATED CORRECTLY FOR RECORDS WHOSE " &
      "COMPONENTS DO NOT HAVE CHANGEABLE " &
      "DISCRIMINANTS");

   if "/=" (Left => Rec1, Right => Rec2) then
      Failed ("INCORRECT RESULTS FOR RECORDS - 1");
   end if;
   Rec1.A := Ident_Int (1);
   if "=" (Left => Rec2, Right => Rec1) then
      Failed ("INCORRECT RESULTS FOR RECORDS - 2");
   end if;

   if Rec3 /= Rec4 then
      Failed ("INCORRECT RESULTS FOR RECORDS - 3");
   end if;
   Rec4.A := Ident_Str ("12345");
   if Rec3 = Rec4 then
      Failed ("INCORRECT RESULTS FOR RECORDS - 4");
   end if;

   Rec5.A := Ident_Str ("WHO");
   Rec6.A := Ident_Str ("WHY");
   if Rec5 = Rec6 then
      Failed ("INCORRECT RESULTS FOR RECORDS - 5");
   end if;
   Rec5.A := "WHY";
   if Rec6 /= Rec5 then
      Failed ("INCORRECT RESULTS FOR RECORDS - 6");
   end if;

   Rec7.A.A    := Ident_Int (1);
   Rec7.A.Bool := (others => Ident_Bool (True));
   Rec8.A.A    := 1;
   Rec8.A.Bool := (others => True);
   if Rec7 /= Rec8 then
      Failed ("INCORRECT RESULTS FOR RECORDS - 7");
   end if;
   Rec8.A.Bool := (others => Ident_Bool (False));
   if Rec8 = Rec7 then
      Failed ("INCORRECT RESULTS FOR RECORDS - 8");
   end if;

   if "/=" (Left => Rec9, Right => Rec10) then
      Failed ("INCORRECT RESULTS FOR RECORDS - 9");
   end if;
   Rec9.A.A := Ident_Int (1);
   if "=" (Left => Rec9, Right => Rec10) then
      Failed ("INCORRECT RESULTS FOR RECORDS - 10");
   end if;

   Result;
end C45271a;
