-- C45282B.ADA

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
--     D) ACCESS TO RECORD, PRIVATE, AND LIMITED PRIVATE TYPES WITH
--        DISCRIMINANTS (WITH AND WITHOUT DEFAULT VALUES), WHERE THE
--        TYPE MARK DENOTES A CONSTRAINED AND UNCONSTRAINED TYPE;
--     E) ACCESS TO TASK TYPES.

-- TBN  8/8/86

with Report; use Report;
procedure C45282b is

   subtype Int is Integer range 1 .. 5;

   package P is
      type Pri_Rec1 (D : Int) is private;
      type Pri_Rec2 (D : Int := 2) is private;
      function Init_Prec1 (A : Int; B : String) return Pri_Rec1;
      function Init_Prec2 (A : Int; B : String) return Pri_Rec2;
      type Lim_Rec1 (D : Int) is limited private;
      type Acc_Lim1 is access Lim_Rec1;
      subtype Acc_Sub_Lim1 is Acc_Lim1 (2);
      procedure Assign_Lim1 (A : Acc_Lim1; B : Int; C : String);
      type Lim_Rec2 (D : Int := 2) is limited private;
      type Acc_Lim2 is access Lim_Rec2;
      subtype Acc_Sub_Lim2 is Acc_Lim2 (2);
      procedure Assign_Lim2 (A : Acc_Lim2; B : Int; C : String);
   private
      type Pri_Rec1 (D : Int) is record
         Str : String (1 .. D);
      end record;
      type Pri_Rec2 (D : Int := 2) is record
         Str : String (1 .. D);
      end record;
      type Lim_Rec1 (D : Int) is record
         Str : String (1 .. D);
      end record;
      type Lim_Rec2 (D : Int := 2) is record
         Str : String (1 .. D);
      end record;
   end P;

   use P;

   type Dis_Rec1 (D : Int) is record
      Str : String (1 .. D);
   end record;
   type Dis_Rec2 (D : Int := 5) is record
      Str : String (D .. 8);
   end record;

   type Acc1_Rec1 is access Dis_Rec1;
   subtype Acc2_Rec1 is Acc1_Rec1 (2);
   type Acc1_Rec2 is access Dis_Rec2;
   subtype Acc2_Rec2 is Acc1_Rec2 (2);
   Rec1 : Acc1_Rec1;
   Rec2 : Acc2_Rec1;
   Rec3 : Acc1_Rec2;
   Rec4 : Acc2_Rec2;
   type Acc_Prec1 is access Pri_Rec1;
   subtype Acc_Srec1 is Acc_Prec1 (2);
   Rec5 : Acc_Prec1;
   Rec6 : Acc_Srec1;
   type Acc_Prec2 is access Pri_Rec2;
   subtype Acc_Srec2 is Acc_Prec2 (2);
   Rec7  : Acc_Prec2;
   Rec8  : Acc_Srec2;
   Rec9  : Acc_Lim1;
   Rec10 : Acc_Sub_Lim1;
   Rec11 : Acc_Lim2;
   Rec12 : Acc_Sub_Lim2;

   task type T is
      entry E (X : Integer);
   end T;

   task body T is
   begin
      accept E (X : Integer) do
         if X /= Ident_Int (1) then
            Failed ("INCORRECT VALUE PASSED TO TASK");
         end if;
      end E;
   end T;

   package body P is
      function Init_Prec1 (A : Int; B : String) return Pri_Rec1 is
         Rec : Pri_Rec1 (A);
      begin
         Rec := (A, B);
         return (Rec);
      end Init_Prec1;

      function Init_Prec2 (A : Int; B : String) return Pri_Rec2 is
         Rec : Pri_Rec2;
      begin
         Rec := (A, B);
         return (Rec);
      end Init_Prec2;

      procedure Assign_Lim1 (A : Acc_Lim1; B : Int; C : String) is
      begin
         A.all := (B, C);
      end Assign_Lim1;

      procedure Assign_Lim2 (A : Acc_Lim2; B : Int; C : String) is
      begin
         A.all := (B, C);
      end Assign_Lim2;
   end P;

begin

   Test
     ("C45282B",
      "CHECK THAT IN AND NOT IN ARE EVALUATED FOR " &
      "ACCESS TYPES TO RECORD TYPES, PRIVATE TYPES, " &
      "LIMITED PRIVATE TYPES WITH DISCRIMINANTS, AND " & "TASK TYPES");

-- CASE D
------------------------------------------------------------------------
   if Rec1 not in Acc1_Rec1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 1");
   end if;
   if Rec1 in Acc2_Rec1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 2");
   end if;
   if Rec2 not in Acc1_Rec1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 3");
   end if;
   Rec1 := new Dis_Rec1'(5, "12345");
   if Rec1 in Acc1_Rec1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 4");
   end if;
   if Rec1 in Acc2_Rec1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 5");
   end if;
   Rec2 := new Dis_Rec1'(2, "HI");
   if Rec2 in Acc1_Rec1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 6");
   end if;

------------------------------------------------------------------------

   if Rec3 in Acc1_Rec2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 7");
   end if;
   if Rec3 not in Acc2_Rec2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 8");
   end if;
   if Rec4 in Acc1_Rec2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 9");
   end if;
   Rec3 := new Dis_Rec2'(5, "5678");
   if Rec3 in Acc1_Rec2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 10");
   end if;
   if Rec3 in Acc2_Rec2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 11");
   end if;
   Rec4 := new Dis_Rec2'(2, "2345678");
   if Rec4 in Acc1_Rec2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 12");
   end if;
   if Rec4 not in Acc2_Rec2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 13");
   end if;

------------------------------------------------------------------------

   if Rec5 not in Acc_Prec1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 14");
   end if;
   if Rec5 not in Acc_Srec1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 15");
   end if;
   if Rec6 not in Acc_Prec1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 16");
   end if;
   Rec5 := new Pri_Rec1'(Init_Prec1 (5, "12345"));
   if Rec5 in Acc_Prec1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 17");
   end if;
   if Rec5 in Acc_Srec1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 18");
   end if;
   Rec6 := new Pri_Rec1'(Init_Prec1 (2, "HI"));
   if Rec6 in Acc_Prec1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 19");
   end if;

------------------------------------------------------------------------

   if Rec7 not in Acc_Prec2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 20");
   end if;
   if Rec7 not in Acc_Srec2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 21");
   end if;
   if Rec8 not in Acc_Prec2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 22");
   end if;
   Rec7 := new Pri_Rec2'(Init_Prec2 (5, "12345"));
   if Rec7 in Acc_Prec2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 23");
   end if;
   if Rec7 in Acc_Srec2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 24");
   end if;
   Rec8 := new Pri_Rec2'(Init_Prec2 (2, "HI"));
   if Rec8 in Acc_Prec2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 25");
   end if;

------------------------------------------------------------------------

   if Rec9 not in Acc_Lim1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 26");
   end if;
   if Rec9 not in Acc_Sub_Lim1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 27");
   end if;
   if Rec10 not in Acc_Lim1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 28");
   end if;
   Rec9 := new Lim_Rec1 (5);
   Assign_Lim1 (Rec9, 5, "12345");
   if Rec9 in Acc_Lim1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 29");
   end if;
   if Rec9 in Acc_Sub_Lim1 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 30");
   end if;
   Rec10 := new Lim_Rec1 (2);
   Assign_Lim1 (Rec10, 2, "12");
   if Rec10 in Acc_Lim1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 31");
   end if;

------------------------------------------------------------------------

   if Rec11 not in Acc_Lim2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 32");
   end if;
   if Rec11 not in Acc_Sub_Lim2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 33");
   end if;
   if Rec12 not in Acc_Lim2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 34");
   end if;
   Rec11 := new Lim_Rec2;
   if Rec11 not in Acc_Sub_Lim2 then
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 35");
   end if;
   Assign_Lim2 (Rec11, 2, "12");
   if Rec11 in Acc_Lim2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 36");
   end if;
   if Rec11 in Acc_Sub_Lim2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 37");
   end if;
   Rec12 := new Lim_Rec2;
   Assign_Lim2 (Rec12, 2, "12");
   if Rec12 in Acc_Lim2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 38");
   end if;

-- CASE E
------------------------------------------------------------------------
   declare
      type Acc_Task is access T;
      T1 : Acc_Task;
   begin
      if T1 not in Acc_Task then
         Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 39");
      end if;
      T1 := new T;
      if T1 in Acc_Task then
         null;
      else
         Failed ("INCORRECT RESULTS FOR ACCESS TYPES - 38");
      end if;
      T1.E (1);
   end;

   Result;
end C45282b;
