-- C74004A.ADA

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
--     CHECK THAT OPERATIONS DEPENDING ON THE FULL DECLARATION OF A
--     PRIVATE TYPE ARE AVAILABLE WITHIN THE PACKAGE BODY.

-- HISTORY:
--     BCB 04/05/88  CREATED ORIGINAL TEST.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C74004a is

   package P is
      type Pr is private;
      type Arr1 is limited private;
      type Arr2 is private;
      type Rec (D : Integer) is private;
      type Acc is private;
      type Tsk is limited private;
      type Flt is limited private;
      type Fix is limited private;

      task type T is
         entry One (V : in out Integer);
      end T;

      procedure Check (V : Arr2);
   private
      type Pr is new Integer;

      type Arr1 is array (1 .. 5) of Integer;

      type Arr2 is array (1 .. 5) of Boolean;

      type Rec (D : Integer) is record
         Comp1 : Integer;
         Comp2 : Boolean;
      end record;

      type Acc is access Integer;

      type Tsk is new T;

      type Flt is digits 5;

      type Fix is delta 2.0**(-1) range -100.0 .. 100.0;
   end P;

   package body P is
      X1, X2, X3 : Pr;
      Bool       : Boolean := Ident_Bool (False);
      Val        : Integer := Ident_Int (0);
      Fval       : Float   := 0.0;
      St         : String (1 .. 2);
      O1         : Arr1    := (1, 2, 3, 4, 5);
      Y1         : Arr2    := (False, True, False, True, False);
      Y2         : Arr2    := (others => True);
      Y3         : Arr2    := (others => False);
      Z1         : Rec (0) := (0, 1, False);
      W1, W2     : Acc     := new Integer'(0);
      V1         : Tsk;

      task body T is
      begin
         accept One (V : in out Integer) do
            V := Ident_Int (10);
         end One;
      end T;

      procedure Check (V : Arr2) is
      begin
         if V /= (True, False, True, False, True) then
            Failed ("IMPROPER VALUE PASSED AS AGGREGATE");
         end if;
      end Check;
   begin
      Test
        ("C74004A",
         "CHECK THAT OPERATIONS DEPENDING ON THE " &
         "FULL DECLARATION OF A PRIVATE TYPE ARE " &
         "AVAILABLE WITHIN THE PACKAGE BODY");

      X1 := 10;
      X2 := 5;

      X3 := X1 + X2;

      if X3 /= 15 then
         Failed ("IMPROPER RESULT FROM ADDITION OPERATOR");
      end if;

      X3 := X1 - X2;

      if X3 /= 5 then
         Failed ("IMPROPER RESULT FROM SUBTRACTION OPERATOR");
      end if;

      X3 := X1 * X2;

      if X3 /= 50 then
         Failed ("IMPROPER RESULT FROM MULTIPLICATION OPERATOR");
      end if;

      X3 := X1 / X2;

      if X3 /= 2 then
         Failed ("IMPROPER RESULT FROM DIVISION OPERATOR");
      end if;

      X3 := X1**2;

      if X3 /= 100 then
         Failed ("IMPROPER RESULT FROM EXPONENTIATION OPERATOR");
      end if;

      Bool := X1 < X2;

      if Bool then
         Failed ("IMPROPER RESULT FROM LESS THAN OPERATOR");
      end if;

      Bool := X1 > X2;

      if not Bool then
         Failed ("IMPROPER RESULT FROM GREATER THAN OPERATOR");
      end if;

      Bool := X1 <= X2;

      if Bool then
         Failed ("IMPROPER RESULT FROM LESS THAN OR EQUAL TO " & "OPERATOR");
      end if;

      Bool := X1 >= X2;

      if not Bool then
         Failed
           ("IMPROPER RESULT FROM GREATER THAN OR EQUAL " & "TO OPERATOR");
      end if;

      X3 := X1 mod X2;

      if X3 /= 0 then
         Failed ("IMPROPER RESULT FROM MOD OPERATOR");
      end if;

      X3 := X1 rem X2;

      if X3 /= 0 then
         Failed ("IMPROPER RESULT FROM REM OPERATOR");
      end if;

      X3 := abs (X1);

      if X3 /= 10 then
         Failed ("IMPROPER RESULT FROM ABS OPERATOR - 1");
      end if;

      X1 := -10;

      X3 := abs (X1);

      if X3 /= 10 then
         Failed ("IMPROPER RESULT FROM ABS OPERATOR - 2");
      end if;

      X3 := Pr'Base'First;

      if X3 /= Pr (Integer'First) then
         Failed ("IMPROPER RESULT FROM 'BASE'FIRST");
      end if;

      X3 := Pr'First;

      if X3 /= Pr (Integer'First) then
         Failed ("IMPROPER RESULT FROM 'FIRST");
      end if;

      Val := Pr'Width;

      if not Equal (Val, Integer'Width) then
         Failed ("IMPROPER RESULT FROM 'WIDTH");
      end if;

      Val := Pr'Pos (X3);

      if not Equal (Val, Integer'First) then
         Failed ("IMPROPER RESULT FROM 'POS");
      end if;

      X3 := Pr'Val (Val);

      if X3 /= Pr (Integer'First) then
         Failed ("IMPROPER RESULT FROM 'VAL");
      end if;

      X3 := Pr'Succ (X2);

      if X3 /= 6 then
         Failed ("IMPROPER RESULT FROM 'SUCC");
      end if;

      X3 := Pr'Pred (X2);

      if X3 /= 4 then
         Failed ("IMPROPER RESULT FROM 'PRED");
      end if;

      St := Pr'Image (X3);

      if St /= Integer'Image (Integer (X3)) then
         Failed ("IMPROPER RESULT FROM 'IMAGE");
      end if;

      X3 := Pr'Value (St);

      if X3 /= Pr (Integer'Value (St)) then
         Failed ("IMPROPER RESULT FROM 'VALUE");
      end if;

      Check ((True, False, True, False, True));

      if O1 (2) /= Ident_Int (2) then
         Failed ("IMPROPER VALUE FROM INDEXING");
      end if;

      if O1 (2 .. 4) /= (2, 3, 4) then
         Failed ("IMPROPER VALUES FROM SLICING");
      end if;

      if Val in O1'Range then
         Failed ("IMPROPER RESULT FROM 'RANGE");
      end if;

      Val := O1'Length;

      if not Equal (Val, 5) then
         Failed ("IMPROPER RESULT FROM 'LENGTH");
      end if;

      Y3 := Y1 (1 .. 2) & Y2 (3 .. 5);

      if Y3 /= (False, True, True, True, True) then
         Failed ("IMPROPER RESULT FROM CATENATION");
      end if;

      Y3 := not Y1;

      if Y3 /= (True, False, True, False, True) then
         Failed ("IMPROPER RESULT FROM NOT OPERATOR");
      end if;

      Y3 := Y1 and Y2;

      if Y3 /= (False, True, False, True, False) then
         Failed ("IMPROPER RESULT FROM AND OPERATOR");
      end if;

      Y3 := Y1 or Y2;

      if Y3 /= (True, True, True, True, True) then
         Failed ("IMPROPER RESULT FROM OR OPERATOR");
      end if;

      Y3 := Y1 xor Y2;

      if Y3 /= (True, False, True, False, True) then
         Failed ("IMPROPER RESULT FROM XOR OPERATOR");
      end if;

      Val := Z1.Comp1;

      if not Equal (Val, 1) then
         Failed ("IMPROPER RESULT FROM SELECTION OF RECORD " & "COMPONENTS");
      end if;

      W1 := new Integer'(0);

      if not Equal (W1.all, 0) then
         Failed ("IMPROPER RESULT FROM ALLOCATION");
      end if;

      W1 := null;

      if W1 /= null then
         Failed ("IMPROPER RESULT FROM NULL LITERAL");
      end if;

      Val := W2.all;

      if not Equal (Val, 0) then
         Failed ("IMPROPER RESULT FROM SELECTED COMPONENT");
      end if;

      Bool := V1'Callable;

      if not Bool then
         Failed ("IMPROPER RESULT FROM 'CALLABLE");
      end if;

      Bool := V1'Terminated;

      if Bool then
         Failed ("IMPROPER RESULT FROM 'TERMINATED");
      end if;

      V1.One (Val);

      if not Equal (Val, 10) then
         Failed ("IMPROPER RESULT RETURNED FROM ENTRY SELECTION");
      end if;

      if not (Flt (1.0) in Flt) then
         Failed ("IMPROPER RESULT FROM IMPLICIT CONVERSION");
      end if;

      Val := Flt'Digits;

      if not Equal (Val, 5) then
         Failed ("IMPROPER RESULT FROM 'DIGITS");
      end if;

      Bool := Flt'Machine_Rounds;

      Bool := Flt'Machine_Overflows;

      Val := Flt'Machine_Radix;

      Val := Flt'Machine_Mantissa;

      Val := Flt'Machine_Emax;

      Val := Flt'Machine_Emin;

      Fval := Fix'Delta;

      if Fval /= 2.0**(-1) then
         Failed ("IMPROPER RESULT FROM 'DELTA");
      end if;

      Val := Fix'Fore;

      Val := Fix'Aft;

   end P;

   use P;

begin
   Result;
end C74004a;
