-- C45265A.ADA

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
-- CHECK THAT MEMBERSHIP TESTS YIELD THE CORRECT RESULTS FOR ONE DIMENSIONAL
-- AND MULTI-DIMENSIONAL ARRAY TYPES WHEN:
--     A) THE SUBTYPE INDICATION DENOTES AN UNCONSTRAINED ARRAY.
--     B) THE SUBTYPE INDICATION DENOTES A CONSTRAINED ARRAY.

-- TBN  7/22/86

with Report; use Report;
procedure C45265a is

   package P is
      type Key is limited private;
   private
      type Key is new Natural;
   end P;

   subtype Int is Integer range 1 .. 20;
   type Array_Type_1 is array (Int range <>) of Integer;
   type Array_Type_2 is array (Int range <>, Int range <>) of Integer;
   type Array_Type_3 is
     array (Int range <>, Int range <>, Int range <>) of Integer;
   type Array_Type_4 is array (Int range <>) of P.Key;
   type Array_Type_5 is array (Int range <>, Int range <>) of P.Key;

   subtype Array_Sub1 is Array_Type_1;
   subtype Array_Sub2 is Array_Type_2;
   subtype Array_Sub3 is Array_Type_3;
   subtype Array_Sub4 is Array_Type_4;
   subtype Array_Sub5 is Array_Type_5;
   subtype Con_Array1 is Array_Type_1 (1 .. 5);
   subtype Con_Array2 is Array_Type_2 (1 .. 2, 1 .. 2);
   subtype Con_Array3 is Array_Type_3 (1 .. 2, 1 .. 3, 1 .. 4);
   subtype Con_Array4 is Array_Type_4 (1 .. 4);
   subtype Con_Array5 is Array_Type_5 (1 .. 2, 1 .. 3);
   subtype Null_Array1 is Array_Type_1 (2 .. 1);

   Array1       : Array_Type_1 (1 .. 10);
   Array2       : Array_Sub1 (11 .. 20);
   Array3       : Array_Type_2 (1 .. 4, 1 .. 3);
   Array4       : Array_Sub2 (5 .. 7, 5 .. 8);
   Array5       : Array_Type_3 (1 .. 2, 1 .. 3, 1 .. 4);
   Array6       : Array_Sub3 (1 .. 3, 1 .. 2, 1 .. 4);
   Null_Array_1 : Array_Type_1 (3 .. 2);
   Null_Array_2 : Array_Sub1 (2 .. 1);
   Array7       : Array_Type_1 (1 .. 10)                   := (1 .. 10 => 7);
   Array8       : Con_Array1                               := (1 .. 5 => 8);
   Array9 : Array_Type_2 (1 .. 10, 1 .. 10) := (1 .. 10 => (1 .. 10 => 9));
   Array10      : Con_Array2 := (1 .. 2 => (1 .. 2 => 10));
   Array11      : Array_Type_3 (1 .. 10, 1 .. 10, 1 .. 10) :=
     (1 .. 10 => (1 .. 10 => (1 .. 10 => 11)));
   Array12 : Con_Array3 := (1 .. 2 => (1 .. 3 => (1 .. 4 => 12)));
   Array13 : Array_Type_4 (1 .. 2);
   Array14 : Array_Sub4 (1 .. 5);
   Array15 : Array_Type_4 (1 .. 6);
   Array16 : Con_Array4;
   Array17 : Array_Type_5 (1 .. 3, 1 .. 2);
   Array18 : Array_Sub5 (1 .. 2, 1 .. 3);
   Array19 : Array_Type_5 (1 .. 4, 1 .. 3);
   Array20 : Con_Array5;

begin
   Test
     ("C45265A",
      "CHECK THAT MEMBERSHIP TESTS YIELD THE CORRECT " &
      "RESULTS FOR ONE DIMENSIONAL AND MULTI-" & "DIMENSIONAL ARRAY TYPES");

   Array1 := (Array1'Range => 1);
   Array2 := (Array2'Range => 2);
   Array3 := (Array3'Range (1) => (Array3'Range (2) => 3));
   Array4 := (Array4'Range (1) => (Array4'Range (2) => 4));
   Array5 :=
     (Array5'Range (1) => (Array5'Range (2) => (Array5'Range (3) => 5)));
   Array6 :=
     (Array6'Range (1) => (Array6'Range (2) => (Array6'Range (3) => 6)));

   if Array1 in Array_Sub1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 1");
   end if;
   if Array2 not in Array_Sub1 then
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 2");
   end if;

   if Array3 in Array_Sub2 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 3");
   end if;
   if Array4 not in Array_Sub2 then
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 4");
   end if;

   if Array5 in Array_Sub3 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 5");
   end if;
   if Array6 not in Array_Sub3 then
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 6");
   end if;

   if Null_Array_1 in Array_Sub1 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 7");
   end if;
   if Null_Array_2 not in Array_Sub1 then
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 8");
   end if;

   if Array7 in Con_Array1 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 9");
   end if;
   if Array8 not in Con_Array1 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 10");
   end if;

   if Array9 in Con_Array2 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 11");
   end if;
   if Array10 not in Con_Array2 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 12");
   end if;

   if Array11 in Con_Array3 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 13");
   end if;
   if Array12 not in Con_Array3 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 14");
   end if;

   if Array13 in Array_Sub4 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 15");
   end if;
   if Array14 not in Array_Sub4 then
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 16");
   end if;

   if Array15 in Con_Array4 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 17");
   end if;
   if Array16 not in Con_Array4 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 18");
   end if;

   if Array17 in Array_Sub5 then
      null;
   else
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 19");
   end if;
   if Array18 not in Array_Sub5 then
      Failed ("INCORRECT RESULTS FOR UNCONSTRAINED ARRAYS - 20");
   end if;

   if Array19 in Con_Array5 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 21");
   end if;
   if Array20 not in Con_Array5 then
      Failed ("INCORRECT RESULTS FOR CONSTRAINED ARRAYS - 22");
   end if;

   if Null_Array_1 in Null_Array1 then
      Failed ("INCORRECT RESULTS FOR NULL ARRAYS - 23");
   end if;
   if Null_Array_2 not in Null_Array1 then
      Failed ("INCORRECT RESULTS FOR NULL ARRAYS - 24");
   end if;

   Result;
end C45265a;
