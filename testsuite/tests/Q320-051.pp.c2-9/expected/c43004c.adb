-- C43004C.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE VALUE OF A
--     DISCRIMINANT OF A CONSTRAINED COMPONENT OF AN AGGREGATE DOES
--     NOT EQUAL THE CORRESPONDING DISCRIMINANT VALUE FOR THE
--     COMPONENT'S SUBTYPE.

-- HISTORY:
--     BCB 07/19/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C43004c is

   Zero : Integer := 0;

   type Rec (D : Integer := 0) is record
      Comp1 : Integer;
   end record;

   type Drec (Dd : Integer := Zero) is record
      Dcomp1 : Integer;
   end record;

   type Rec1 is record
      A : Rec (0);
   end record;

   type Rec2 is record
      B : Drec (Zero);
   end record;

   type Rec3 (D3 : Integer := 0) is record
      C : Rec (D3);
   end record;

   V : Rec1;
   W : Rec2;
   X : Rec3;

   package P is
      type Priv1 (D : Integer := 0) is private;
      type Priv2 (Dd : Integer := Zero) is private;
      function Init (I : Integer) return Priv1;
   private
      type Priv1 (D : Integer := 0) is record
         null;
      end record;

      type Priv2 (Dd : Integer := Zero) is record
         null;
      end record;
   end P;

   type Rec7 is record
      H : P.Priv1 (0);
   end record;

   Y : Rec7;

   generic
      type Gp is private;
   function Gen_Equal (X, Y : Gp) return Boolean;

   function Gen_Equal (X, Y : Gp) return Boolean is
   begin
      return X = Y;
   end Gen_Equal;

   package body P is
      type Rec4 is record
         E : Priv1 (0);
      end record;

      type Rec5 is record
         F : Priv2 (Zero);
      end record;

      type Rec6 (D6 : Integer := 0) is record
         G : Priv1 (D6);
      end record;

      Vv : Rec4;
      Ww : Rec5;
      Xx : Rec6;

      function Rec4_Equal is new Gen_Equal (Rec4);
      function Rec5_Equal is new Gen_Equal (Rec5);
      function Rec6_Equal is new Gen_Equal (Rec6);

      function Init (I : Integer) return Priv1 is
         Var : Priv1;
      begin
         Var := (D => I);
         return Var;
      end Init;
   begin
      Test
        ("C43004C",
         "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
         "IF THE VALUE OF A DISCRIMINANT OF A " &
         "CONSTRAINED COMPONENT OF AN AGGREGATE " &
         "DOES NOT EQUAL THE CORRESPONDING " &
         "DISCRIMINANT VALUE FOR THECOMPONENT'S " & "SUBTYPE");

      begin
         Vv := (E => (D => 1));
         Failed ("CONSTRAINT_ERROR NOT RAISED - 1");
         if Rec4_Equal (Vv, Vv) then
            Comment ("DON'T OPTIMIZE VV");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 1");
      end;

      begin
         Ww := (F => (Dd => 1));
         Failed ("CONSTRAINT_ERROR NOT RAISED - 2");
         if Rec5_Equal (Ww, Ww) then
            Comment ("DON'T OPTIMIZE WW");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 2");
      end;

      begin
         Xx := (D6 => 1, G => (D => 5));
         Failed ("CONSTRAINT_ERROR NOT RAISED - 3");
         if Rec6_Equal (Xx, Xx) then
            Comment ("DON'T OPTIMIZE XX");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED - 3");
      end;
   end P;

   use P;

   function Rec1_Equal is new Gen_Equal (Rec1);
   function Rec2_Equal is new Gen_Equal (Rec2);
   function Rec3_Equal is new Gen_Equal (Rec3);
   function Rec7_Equal is new Gen_Equal (Rec7);

begin

   begin
      V := (A => (D => 1, Comp1 => 2));
      Failed ("CONSTRAINT_ERROR NOT RAISED - 4");
      if Rec1_Equal (V, V) then
         Comment ("DON'T OPTIMIZE V");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED - 4");
   end;

   begin
      W := (B => (Dd => 1, Dcomp1 => 2));
      Failed ("CONSTRAINT_ERROR NOT RAISED - 5");
      if Rec2_Equal (W, W) then
         Comment ("DON'T OPTIMIZE W");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED - 5");
   end;

   begin
      X := (D3 => 1, C => (D => 5, Comp1 => 2));
      Failed ("CONSTRAINT_ERROR NOT RAISED - 6");
      if Rec3_Equal (X, X) then
         Comment ("DON'T OPTIMIZE X");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED - 6");
   end;

   begin
      Y := (H => Init (1));
      Failed ("CONSTRAINT_ERROR NOT RAISED - 7");
      if Rec7_Equal (Y, Y) then
         Comment ("DON'T OPTIMIZE Y");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED - 7");
   end;

   Result;
end C43004c;
