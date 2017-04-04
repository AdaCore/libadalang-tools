-- C38102E.ADA

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
-- CHECK THAT AN INCOMPLETE TYPE CAN BE REDECLARED AS A DERIVED GENERIC FORMAL
-- TYPE.

-- AH     8/15/86
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X. DNT 11/28/95 CHANGED TO
-- FLAG1 := F4.

with Report; use Report;
procedure C38102e is
   type Rainbow is (Red, Orange, Yellow, Green, Blue, Indigo, Violet);
   type T_Float is digits 5 range -4.0 .. 4.0;
   type T_Fixed is delta 0.01 range 0.0 .. 1.5;
   subtype P1 is Integer;
   type P2 is range 0 .. 10;
   type P3 is array (P2) of Integer;
   type P4 is array (P2, P2) of Integer;

   F1, F2 : Boolean;

   generic
      type G1 is (<>);
      type G2 is range <>;
   function G_Discrete return Boolean;

   function G_Discrete return Boolean is
      type Inc1;
      type Inc2;
      type F1 is new G1;
      type Inc1 is new G1;
      type Inc2 is new G2;

      Obj1_0 : Inc1;
      Obj1_1 : Inc1;
      Obj2_0 : Inc2;
      Obj2_1 : Inc2;
      Obj3   : F1;

      Result_Value1 : Boolean := False;
      Result_Value2 : Boolean := False;
   begin
      Obj3 := F1'Last;
      Obj3 := F1'Pred (Obj3);
      if Inc1 (Obj3) = Inc1'Pred (Inc1'Last) then
         Result_Value1 := True;
      end if;
      Obj2_0 := Inc2'First;
      Obj2_1 := Inc2'Last;
      if (Obj2_0 + Obj2_1) = (Inc2'Succ (Obj2_0) + Inc2'Pred (Obj2_1)) then
         Result_Value2 := True;
      end if;

      return (Result_Value1 and Result_Value2);
   end G_Discrete;

   generic
      type G3 is digits <>;
      type G4 is delta <>;
   procedure Reals (Flag1, Flag2 : out Boolean);

   procedure Reals (Flag1, Flag2 : out Boolean) is
      F1, F2, F3, F4, F5, F6, F7, F8 : Boolean;
      type Inc3;
      type Inc4;
      type P1 is new G3;
      type P2 is new G4;
      type Inc3 is new G3;
      type Inc4 is new G4;
   begin
      F4 := P1'Last = P1 (Inc3'Last) and P1'First = P1 (Inc3'First);

      F5 := P2'Fore = Inc4'Fore;
      F6 := P2'Aft = Inc4'Aft;
      F7 := abs (P2'Last - P2'First) = P2 (abs (Inc4'Last - Inc4'First));
      F8 := Inc4 (P2'Last / P2'Last) = Inc4 (Inc4'Last / Inc4'Last);

      Flag1 := F4;
      Flag2 := F5 and F6 and F7 and F8;
   end Reals;

   generic
      type Item is private;
      type Index is range <>;
      type G5 is array (Index) of Item;
      type G6 is array (Index, Index) of Item;
   package Dimensions is
      type Inc5;
      type Inc6;
      type D1 is new G5;
      type D2 is new G6;
      type Inc5 is new G5;
      type Inc6 is new G6;
      function Check return Boolean;
   end Dimensions;

   package body Dimensions is
      function Check return Boolean is
         A1     : Inc5;
         A2     : Inc6;
         Dim1   : D1;
         Dim2   : D2;
         F1, F2 : Boolean;
      begin
         F1 := A1 (Index'First)'Size = Dim1 (Index'First)'Size;
         F2 :=
           A2 (Index'First, Index'Last)'Size =
           Dim2 (Index'First, Index'Last)'Size;

         return (F1 and F2);
      end Check;
   end Dimensions;

   procedure Proc is new Reals (G3 => T_Float, G4 => T_Fixed);
   function Discrete is new G_Discrete (G1 => Rainbow, G2 => P2);
   package Pkg is new Dimensions (Item => P1, Index => P2, G5 => P3, G6 => P4);

   use Pkg;
begin
   Test
     ("C38102E",
      "INCOMPLETE TYPES CAN BE DERIVED GENERIC " & "FORMAL TYPES");

   if not Discrete then
      Failed ("INTEGER AND ENUMERATED TYPES NOT DERIVED");
   end if;

   Proc (F1, F2);
   if (not F1) then
      Failed ("FLOAT TYPES NOT DERIVED");
   end if;
   if (not F2) then
      Failed ("FIXED TYPES NOT DERIVED");
   end if;

   if not Check then
      Failed ("ONE AND TWO DIMENSIONAL ARRAY TYPES NOT DERIVED");
   end if;

   Result;
end C38102e;
