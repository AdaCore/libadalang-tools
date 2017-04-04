-- C74302B.ADA

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
--     CHECK THAT WHEN THE FULL DECLARATION OF A DEFERRED CONSTANT IS
--     GIVEN AS A MULTIPLE DECLARATION, THE INITIALIZATION EXPRESSION
--     IS EVALUATED ONCE FOR EACH DEFERRED CONSTANT.  (USE ENUMERATION,
--     INTEGER, FIXED POINT, FLOATING POINT, ARRAY, RECORD (INCLUDING
--     USE OF DEFAULT EXPRESSIONS FOR COMPONENTS), ACCESS, AND PRIVATE
--     TYPES AS FULL DECLARATION OF PRIVATE TYPE)

-- HISTORY:
--     BCB 07/25/88  CREATED ORIGINAL TEST.
--     RLB 03/19/07  ELIMINATED INCOMPABILITY WITH AMENDMENT 1.

with Report; use Report;

procedure C74302b is

   type Arr_Ran is range 1 .. 2;

   Bump : Integer := Ident_Int (0);

   generic
      type Dt is (<>);
   function F1 return Dt;

   generic
      type Fe is delta <>;
   function F2 return Fe;

   generic
      type Fle is digits <>;
   function F3 return Fle;

   generic
      type Ca is array (Arr_Ran) of Integer;
   function F4 return Ca;

   generic
      type Gp is private;
   function F5 (V : Gp) return Gp;

   generic
      type Gp1 is private;
   function F6 (V1 : Gp1) return Gp1;

   generic
      type Ac is access Integer;
   function F7 return Ac;

   generic
      type Pp is private;
   function F8 (P1 : Pp) return Pp;

   function F1 return Dt is
   begin
      Bump := Bump + 1;
      return Dt'Val (Bump);
   end F1;

   function F2 return Fe is
   begin
      Bump := Bump + 1;
      return Fe (Bump);
   end F2;

   function F3 return Fle is
   begin
      Bump := Bump + 1;
      return Fle (Bump);
   end F3;

   function F4 return Ca is
   begin
      Bump := Bump + 1;
      return ((Bump, Bump - 1));
   end F4;

   function F5 (V : Gp) return Gp is
   begin
      Bump := Bump + 1;
      return V;
   end F5;

   function F6 (V1 : Gp1) return Gp1 is
   begin
      Bump := Bump + 1;
      return V1;
   end F6;

   function F7 return Ac is
      Var : Ac;
   begin
      Bump := Bump + 1;
      Var  := new Integer'(Bump);
      return Var;
   end F7;

   function F8 (P1 : Pp) return Pp is
   begin
      Bump := Bump + 1;
      return P1;
   end F8;

   package Pack is
      type Sp is private;
      Cons : constant Sp;
   private
      type Sp is range 1 .. 100;
      Cons : constant Sp := 50;
   end Pack;

   use Pack;

   package P is
      type Int is private;
      type Enum is private;
      type Fix is private;
      type Flt is private;
      type Con_Arr is private;
      type Rec is private;
      type Rec1 is private;
      type Acc is private;
      type Priv is private;

      generic
         type Lp is private;
      function Gen_Equal (Z1, Z2 : Lp) return Boolean;

      I1, I2, I3, I4     : constant Int;
      E1, E2, E3, E4     : constant Enum;
      Fi1, Fi2, Fi3, Fi4 : constant Fix;
      Fl1, Fl2, Fl3, Fl4 : constant Flt;
      Ca1, Ca2, Ca3, Ca4 : constant Con_Arr;
      R1, R2, R3, R4     : constant Rec;
      R1a, R2a, R3a, R4a : constant Rec1;
      A1, A2, A3, A4     : constant Acc;
      Pr1, Pr2, Pr3, Pr4 : constant Priv;
   private
      type Int is range 1 .. 100;

      type Enum is (One, Two, Three, Four, Five, Six, Seven, Eight, Nine);

      type Fix is delta 2.0**(-1) range -100.0 .. 100.0;

      type Flt is digits 5 range -100.0 .. 100.0;

      type Con_Arr is array (Arr_Ran) of Integer;

      type Rec is record
         Comp1 : Integer;
         Comp2 : Integer;
         Comp3 : Boolean;
      end record;

      type Rec1 is record
         Comp1 : Integer := 10;
         Comp2 : Integer := 20;
         Comp3 : Boolean := False;
      end record;

      type Acc is access Integer;

      type Priv is new Sp;

      function Ddt is new F1 (Int);
      function Edt is new F1 (Enum);
      function Fdt is new F2 (Fix);
      function Fldt is new F3 (Flt);
      function Cadt is new F4 (Con_Arr);
      function Rdt is new F5 (Rec);
      function R1dt is new F6 (Rec1);
      function Adt is new F7 (Acc);
      function Pdt is new F8 (Priv);

      Rec_Obj  : Rec  := (1, 2, True);
      Rec1_Obj : Rec1 := (3, 4, False);

      I1, I2, I3, I4     : constant Int     := Ddt;
      E1, E2, E3, E4     : constant Enum    := Edt;
      Fi1, Fi2, Fi3, Fi4 : constant Fix     := Fdt;
      Fl1, Fl2, Fl3, Fl4 : constant Flt     := Fldt;
      Ca1, Ca2, Ca3, Ca4 : constant Con_Arr := Cadt;
      R1, R2, R3, R4     : constant Rec     := Rdt (Rec_Obj);
      R1a, R2a, R3a, R4a : constant Rec1    := R1dt (Rec1_Obj);
      A1, A2, A3, A4     : constant Acc     := Adt;
      Pr1, Pr2, Pr3, Pr4 : constant Priv    := Pdt (Priv (Cons));
   end P;

   package body P is
      Avar1 : Acc := new Integer'(29);
      Avar2 : Acc := new Integer'(30);
      Avar3 : Acc := new Integer'(31);
      Avar4 : Acc := new Integer'(32);

      function Gen_Equal (Z1, Z2 : Lp) return Boolean is
      begin
         return Z1 = Z2;
      end Gen_Equal;

      function Int_Equal is new Gen_Equal (Int);
      function Enum_Equal is new Gen_Equal (Enum);
      function Fix_Equal is new Gen_Equal (Fix);
      function Flt_Equal is new Gen_Equal (Flt);
      function Arr_Equal is new Gen_Equal (Con_Arr);
      function Rec_Equal is new Gen_Equal (Rec);
      function Rec1_Equal is new Gen_Equal (Rec1);
      function Acc_Equal is new Gen_Equal (Integer);
      function Priv_Equal is new Gen_Equal (Priv);
   begin
      Test
        ("C74302B",
         "CHECK THAT WHEN THE FULL DECLARATION OF " &
         "A DEFERRED CONSTANT IS GIVEN AS A " &
         "MULTIPLE DECLARATION, THE INITIALIZATION " &
         "EXPRESSION IS EVALUATED ONCE FOR EACH " &
         "DEFERRED CONSTANT");

      if not Equal (Bump, 36) then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED CONSTANTS IN A MULIPLE DECLARATION");
      end if;

      if not Int_Equal (I1, 1) or
        not Int_Equal (I2, 2) or
        not Int_Equal (I3, 3) or
        not Int_Equal (I4, 4)
      then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED INTEGER CONSTANTS");
      end if;

      if not Enum_Equal (E1, Six) or
        not Enum_Equal (E2, Seven) or
        not Enum_Equal (E3, Eight) or
        not Enum_Equal (E4, Nine)
      then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED ENUMERATION CONSTANTS");
      end if;

      if not Fix_Equal (Fi1, 9.0) or
        not Fix_Equal (Fi2, 10.0) or
        not Fix_Equal (Fi3, 11.0) or
        not Fix_Equal (Fi4, 12.0)
      then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED FIXED POINT CONSTANTS");
      end if;

      if not Flt_Equal (Fl1, 13.0) or
        not Flt_Equal (Fl2, 14.0) or
        not Flt_Equal (Fl3, 15.0) or
        not Flt_Equal (Fl4, 16.0)
      then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED FLOATING POINT CONSTANTS");
      end if;

      if not Arr_Equal (Ca1, (17, 16)) or
        not Arr_Equal (Ca2, (18, 17)) or
        not Arr_Equal (Ca3, (19, 18)) or
        not Arr_Equal (Ca4, (20, 19))
      then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED ARRAY CONSTANTS");
      end if;

      if not Rec_Equal (R1, Rec_Obj) or
        not Rec_Equal (R2, Rec_Obj) or
        not Rec_Equal (R3, Rec_Obj) or
        not Rec_Equal (R4, Rec_Obj)
      then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED RECORD CONSTANTS");
      end if;

      if not Rec1_Equal (R1a, Rec1_Obj) or
        not Rec1_Equal (R2a, Rec1_Obj) or
        not Rec1_Equal (R3a, Rec1_Obj) or
        not Rec1_Equal (R4a, Rec1_Obj)
      then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED RECORD CONSTANTS WITH DEFAULT " &
            "EXPRESSIONS");
      end if;

      if not Acc_Equal (A1.all, Avar1.all) or
        not Acc_Equal (A2.all, Avar2.all) or
        not Acc_Equal (A3.all, Avar3.all) or
        not Acc_Equal (A4.all, Avar4.all)
      then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED ACCESS CONSTANTS");
      end if;

      if not Priv_Equal (Pr1, Priv (Cons)) or
        not Priv_Equal (Pr2, Priv (Cons)) or
        not Priv_Equal (Pr3, Priv (Cons)) or
        not Priv_Equal (Pr4, Priv (Cons))
      then
         Failed
           ("IMPROPER RESULTS FROM INITIALIZATION OF " &
            "DEFERRED PRIVATE CONSTANTS");
      end if;

      Result;
   end P;

   use P;

begin
   null;
end C74302b;
