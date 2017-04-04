-- C74306A.ADA

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
--     AFTER THE FULL DECLARATION OF A DEFERRED CONSTANT, THE VALUE OF
--     THE CONSTANT MAY BE USED IN ANY EXPRESSION, PARTICULARLY
--     EXPRESSIONS IN WHICH THE USE WOULD BE ILLEGAL BEFORE THE FULL
--     DECLARATION.

-- HISTORY:
--     BCB 03/14/88  CREATED ORIGINAL TEST.
--     RLB 03/19/07  ELIMINATED INCOMPABILITY WITH AMENDMENT 1.

with Report; use Report;

procedure C74306a is

   generic
      type General_Purpose is private;
      Y : in out General_Purpose;
   function Ident (X : General_Purpose) return General_Purpose;

   function Ident (X : General_Purpose) return General_Purpose is
   begin
      if Equal (3, 3) then
         return X;
      end if;
      return Y;
   end Ident;

   package P is
      type T is private;
      C : constant T;
   private
      type T is range 1 .. 100;

      type A is array (1 .. 2) of T;

      type B is array (Integer range <>) of T;

      type D (Disc : T) is record
         null;
      end record;

      C : constant T := 50;

      Param : T := 99;

      function Ident_T is new Ident (T, Param);

      function F (X : T := C) return T;

      subtype Ran is T range 1 .. C;

      subtype Ind is B (1 .. Integer (C));

      subtype Dis is D (Disc => C);

      Obj : T := C;

      Con : constant T := C;

      Arr : A := (5, C);

      Par : T := Ident_T (C);

      Ranobj : T range 1 .. C := C;

      Indobj : B (1 .. Integer (C));

      Dis_Val : Dis;

      Ren : T renames C;

      generic
         For_Par : T := C;
      package Genpack is
         Val : T;
      end Genpack;

      generic
         In_Par : in T;
      package Newpack is
         In_Val : T;
      end Newpack;
   end P;

   use P;

   package body P is
      type A1 is array (1 .. 2) of T;

      type B1 is array (Integer range <>) of T;

      type D1 (Disc1 : T) is record
         null;
      end record;

      subtype Ran1 is T range 1 .. C;

      subtype Ind1 is B1 (1 .. Integer (C));

      subtype Dis1 is D1 (Disc1 => C);

      Obj1 : T := C;

      Funcvar : T;

      Con1 : constant T := C;

      Arr1 : A1 := (5, C);

      Par1 : T := Ident_T (C);

      Ranobj1 : T range 1 .. C := C;

      Indobj1 : B1 (1 .. Integer (C));

      Dis_Val1 : Dis1;

      Ren1 : T renames C;

      function F (X : T := C) return T is
      begin
         return C;
      end F;

      package body Genpack is
      begin
         Val := For_Par;
      end Genpack;

      package body Newpack is
      begin
         In_Val := In_Par;
      end Newpack;

      package Pack is new Genpack (For_Par => C);

      package Npack is new Newpack (In_Par => C);
   begin
      Test
        ("C74306A",
         "AFTER THE FULL DECLARATION OF A DEFERRED " &
         "CONSTANT, THE VALUE OF THE CONSTANT MAY " &
         "BE USED IN ANY EXPRESSION, PARTICULARLY " &
         "EXPRESSIONS IN WHICH THE USE WOULD BE " &
         "ILLEGAL BEFORE THE FULL DECLARATION");

      if Obj /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR OBJ");
      end if;

      if Con /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR CON");
      end if;

      if Arr /= (Ident_T (5), Ident_T (50)) then
         Failed ("IMPROPER VALUES FOR ARR");
      end if;

      if Par /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR PAR");
      end if;

      if Obj1 /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR OBJ1");
      end if;

      if Con1 /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR CON1");
      end if;

      if Arr1 /= (Ident_T (5), Ident_T (50)) then
         Failed ("IMPROPER VALUES FOR ARR1");
      end if;

      if Par1 /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR PAR1");
      end if;

      if Pack.Val /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR PACK.VAL");
      end if;

      if Npack.In_Val /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR NPACK.IN_VAL");
      end if;

      if Ran'Last /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR RAN'LAST");
      end if;

      if Ranobj /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR RANOBJ");
      end if;

      if Ind'Last /= Ident_Int (50) then
         Failed ("IMPROPER VALUE FOR IND'LAST");
      end if;

      if Indobj'Last /= Ident_Int (50) then
         Failed ("IMPROPER VALUE FOR INDOBJ'LAST");
      end if;

      if Dis_Val.Disc /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR DIS_VAL.DISC");
      end if;

      if Ren /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR REN");
      end if;

      if Ran1'Last /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR RAN1'LAST");
      end if;

      if Ranobj1 /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR RANOBJ1");
      end if;

      if Ind1'Last /= Ident_Int (50) then
         Failed ("IMPROPER VALUE FOR IND1'LAST");
      end if;

      if Indobj1'Last /= Ident_Int (50) then
         Failed ("IMPROPER VALUE FOR INDOBJ1'LAST");
      end if;

      if Dis_Val1.Disc1 /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR DIS_VAL1.DISC1");
      end if;

      if Ren1 /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR REN1");
      end if;

      Funcvar := F (C);

      if Funcvar /= Ident_T (50) then
         Failed ("IMPROPER VALUE FOR FUNCVAR");
      end if;

      Result;
   end P;

begin
   declare
      type Arr is array (1 .. 2) of T;

      Val1 : T := C;

      Val2 : Arr := (C, C);

      Val3 : T renames C;
   begin
      null;
   end;

   null;
end C74306a;
