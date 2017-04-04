-- C58005H.ADA

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
-- CHECK THAT CONSTRAINTS ON THE RETURN VALUE OF A FUNCTION ARE SATISIFIED WHEN
-- THE FUNCTION RETURNS CONTROL TO ITS INVOKER.

-- THIS TESTS CHECKS FOR CONSTRAINTS ON CONSTRAINED ACCESS TYPES WITH RECORD,
-- ARRAY, PRIVATE AND LIMITED PRIVATE DESIGNATED TYPES.

-- SPS 3/10/83
-- RLB 6/29/01 - Repaired test to work in the face of aggressive optimizations.
--               The objects must be used, and must be tied somehow to the
--               calls to Failed.

with Report; use Report;
procedure C58005h is

   package Pack is
      type Pv (D : Natural) is private;
      type Lp (D : Natural) is limited private;
   private
      type Pv (D : Natural) is record
         null;
      end record;
      type Lp (D : Natural) is record
         null;
      end record;
   end Pack;

   use Pack;

   type Arr is array (Natural range <>) of Natural;
   type Rec (D : Natural) is record
      null;
   end record;

   type Acc_Rec is access Rec;
   type Acc_Arr is access Arr;
   type Acc_Pv is access Pv;
   type Acc_Lp is access Lp;

   subtype Acc_Rec1 is Acc_Rec (D => 1);
   subtype Acc_Rec2 is Acc_Rec (D => 2);

   subtype Acc_Arr1 is Acc_Arr (1 .. 10);
   subtype Acc_Arr2 is Acc_Arr (2 .. 5);

   subtype Acc_Pv1 is Acc_Pv (D => 1);
   subtype Acc_Pv2 is Acc_Pv (D => 2);

   subtype Acc_Lp1 is Acc_Lp (D => 1);
   subtype Acc_Lp2 is Acc_Lp (D => 2);

   Var1 : Acc_Rec1 := new Rec (1);
   Var2 : Acc_Rec2 := new Rec (2);
   Vaa1 : Acc_Arr1 := new Arr (1 .. 10);
   Vaa2 : Acc_Arr2 := new Arr (2 .. 5);
   Vap1 : Acc_Pv1  := new Pv (1);
   Vap2 : Acc_Pv2  := new Pv (2);
   Val1 : Acc_Lp1  := new Lp (1);
   Val2 : Acc_Lp2  := new Lp (2);

   function Frec (X : Acc_Rec1) return Acc_Rec2 is
   begin
      return X;
   end Frec;

   function Farr (X : Acc_Arr1) return Acc_Arr2 is
   begin
      return X;
   end Farr;

   function Fpv (X : Acc_Pv1) return Acc_Pv2 is
   begin
      return X;
   end Fpv;

   function Flp (X : Acc_Lp1) return Acc_Lp2 is
   begin
      return X;
   end Flp;

   package body Pack is
      function Lf (X : Lp) return Integer is
      begin
         return Ident_Int (3);
      end Lf;
   begin
      null;
   end Pack;

begin

   Test
     ("C58005H",
      "CHECK ACCESS CONSTRAINTS ON RETURN VALUES " & "OF FUNCTIONS");

   begin
      Var2 := Frec (Var1);
      if Var2.D /= Report.Ident_Int (2) then
         Failed ("CONSTRAINT_ERROR NOT RAISED - REC 1");
      else
         Failed ("CONSTRAINT_ERROR NOT RAISED - REC 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - REC");
   end;

   begin
      Vaa2 := Farr (Vaa1);
      if Vaa2'First /= Report.Ident_Int (2) then
         Failed ("CONSTRAINT_ERROR NOT RAISED - ARR 1");
      else
         Failed ("CONSTRAINT_ERROR NOT RAISED - ARR 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ARR");
   end;

   begin
      Vap2 := Fpv (Vap1);
      if Vap2.D /= Report.Ident_Int (2) then
         Failed ("CONSTRAINT_ERROR NOT RAISED - PV 1");
      else
         Failed ("CONSTRAINT_ERROR NOT RAISED - PV 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - PV");
   end;

   begin
      Val2 := Flp (Val1);
      if Val2.D /= Report.Ident_Int (2) then
         Failed ("CONSTRAINT_ERROR NOT RAISED - LP 1");
      else
         Failed ("CONSTRAINT_ERROR NOT RAISED - LP 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - LP");
   end;

   Result;
end C58005h;
