-- C45282A.ADA

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
-- CHECK THAT IN AND NOT IN ARE EVALUATED CORRECTLY FOR :
--     A) ACCESS TO SCALAR TYPES;
--     B) ACCESS TO ARRAY TYPES (CONSTRAINED AND UNCONSTRAINED);
--     C) ACCESS TO RECORD, PRIVATE, AND LIMITED PRIVATE TYPES WITHOUT
--        DISCRIMINANTS;

-- TBN  8/8/86

with Report; use Report;
procedure C45282a is

   package P is
      type Key is private;
      function Init_Key (X : Natural) return Key;
      type Newkey is limited private;
      type Acc_Nkey is access Newkey;
      procedure Assign_Newkey (Y : in out Acc_Nkey);
   private
      type Key is new Natural;
      type Newkey is new Key;
   end P;

   use P;
   subtype I is Integer;
   type Acc_Int is access I;
   P_Int : Acc_Int;
   subtype Int is Integer range 1 .. 5;
   type Array_Type1 is array (Int range <>) of Integer;
   type Acc_Ara_1 is access Array_Type1;
   subtype Acc_Ara_2 is Acc_Ara_1 (1 .. 2);
   subtype Acc_Ara_3 is Acc_Ara_1 (1 .. 3);
   Ara1 : Acc_Ara_1;
   Ara2 : Acc_Ara_2;
   Ara3 : Acc_Ara_3;
   type Greet is record
      Name : String (1 .. 2);
   end record;
   type Acc_Greet is access Greet;
   Intro : Acc_Greet;
   type Acc_Key is access Key;
   Key1 : Acc_Key;
   Key2 : Acc_Nkey;

   package body P is
      function Init_Key (X : Natural) return Key is
      begin
         return (Key (X));
      end Init_Key;

      procedure Assign_Newkey (Y : in out Acc_Nkey) is
      begin
         Y.all := Newkey (1);
      end Assign_Newkey;
   end P;

begin

   Test
     ("C45282A",
      "CHECK THAT IN AND NOT IN ARE EVALUATED FOR " &
      "ACCESS TYPES TO SCALAR TYPES, ARRAY TYPES, " &
      "RECORD TYPES, PRIVATE TYPES, AND LIMITED " &
      "PRIVATE TYPES WITHOUT DISCRIMINANTS");

-- CASE A
   if P_Int not in Acc_Int then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 1");
   end if;
   P_Int := new Int'(5);
   if P_Int in Acc_Int then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 2");
   end if;

-- CASE B
   if Ara1 not in Acc_Ara_1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 3");
   end if;
   if Ara1 not in Acc_Ara_2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 4");
   end if;
   if Ara1 in Acc_Ara_3 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 5");
   end if;
   if Ara2 in Acc_Ara_1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 6");
   end if;
   if Ara3 not in Acc_Ara_1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 7");
   end if;
   Ara1 := new Array_Type1'(1, 2, 3);
   if Ara1 in Acc_Ara_1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 8");
   end if;
   if Ara1 in Acc_Ara_2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 9");
   end if;
   if Ara1 not in Acc_Ara_3 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 10");
   end if;
   Ara2 := new Array_Type1'(1, 2);
   if Ara2 not in Acc_Ara_1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 11");
   end if;
   if Ara2 not in Acc_Ara_2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 12");
   end if;

-- CASE C
   if Intro not in Acc_Greet then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 13");
   end if;
   Intro := new Greet'(Name => "HI");
   if Intro in Acc_Greet then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 14");
   end if;
   if Key1 not in Acc_Key then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 15");
   end if;
   Key1 := new Key'(Init_Key (1));
   if Key1 in Acc_Key then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 16");
   end if;
   if Key2 not in Acc_Nkey then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 17");
   end if;
   Key2 := new Newkey;
   Assign_Newkey (Key2);
   if Key2 in Acc_Nkey then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 18");
   end if;

   Result;
end C45282a;
