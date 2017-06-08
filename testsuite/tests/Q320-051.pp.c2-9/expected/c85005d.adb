-- C85005D.ADA

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
--     CHECK THAT A VARIABLE CREATED BY A GENERIC 'IN OUT' FORMAL
--     PARAMETER CAN BE RENAMED AND HAS THE CORRECT VALUE, AND
--     THAT THE NEW NAME CAN BE USED IN AN ASSIGNMENT STATEMENT AND
--     PASSED ON AS AN ACTUAL SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT'
--     PARAMETER, AND AS AN ACTUAL GENERIC 'IN OUT' PARAMETER,
--     AND THAT WHEN THE VALUE OF THE RENAMED VARIABLE IS CHANGED,
--     THE NEW VALUE IS REFLECTED BY THE VALUE OF THE NEW NAME.

-- HISTORY:
--     JET 03/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85005d is

   type Array1 is array (Positive range <>) of Integer;
   type Record1 (D : Integer) is record
      Field1 : Integer := 1;
   end record;
   type Pointer1 is access Integer;

   package Pack1 is
      type Privy is private;
      Zero  : constant Privy;
      One   : constant Privy;
      Two   : constant Privy;
      Three : constant Privy;
      Four  : constant Privy;
      Five  : constant Privy;
      function Ident (I : Privy) return Privy;
      function Next (I : Privy) return Privy;
   private
      type Privy is range 0 .. 127;
      Zero  : constant Privy := 0;
      One   : constant Privy := 1;
      Two   : constant Privy := 2;
      Three : constant Privy := 3;
      Four  : constant Privy := 4;
      Five  : constant Privy := 5;
   end Pack1;

   task type Task1 is
      entry Assign (J : in Integer);
      entry Valu (J : out Integer);
      entry Next;
      entry Stop;
   end Task1;

   Di1 : Integer         := 0;
   Da1 : Array1 (1 .. 3) := (others => 0);
   Dr1 : Record1 (1)     := (D => 1, Field1 => 0);
   Dp1 : Pointer1        := new Integer'(0);
   Dv1 : Pack1.Privy     := Pack1.Zero;
   Dt1 : Task1;

   I : Integer;

   generic
      Gi1 : in out Integer;
      Ga1 : in out Array1;
      Gr1 : in out Record1;
      Gp1 : in out Pointer1;
      Gv1 : in out Pack1.Privy;
      Gt1 : in out Task1;
   package Generic1 is
   end Generic1;

   function Ident (P : Pointer1) return Pointer1 is
   begin
      if Equal (3, 3) then
         return P;
      else
         return null;
      end if;
   end Ident;

   package body Pack1 is
      function Ident (I : Privy) return Privy is
      begin
         if Equal (3, 3) then
            return I;
         else
            return Privy'(0);
         end if;
      end Ident;

      function Next (I : Privy) return Privy is
      begin
         return I + 1;
      end Next;
   end Pack1;

   package body Generic1 is
      Xgi1 : Integer renames Gi1;
      Xga1 : Array1 renames Ga1;
      Xgr1 : Record1 renames Gr1;
      Xgp1 : Pointer1 renames Gp1;
      Xgv1 : Pack1.Privy renames Gv1;
      Xgt1 : Task1 renames Gt1;

      task type Task2 is
         entry Entry1
           (Ti1 :    out Integer;
            Ta1 :    out Array1;
            Tr1 :    out Record1;
            Tp1 : in out Pointer1;
            Tv1 : in out Pack1.Privy;
            Tt1 : in out Task1);
      end Task2;

      G_Chk_Task : Task2;

      generic
         Ggi1 : in out Integer;
         Gga1 : in out Array1;
         Ggr1 : in out Record1;
         Ggp1 : in out Pointer1;
         Ggv1 : in out Pack1.Privy;
         Ggt1 : in out Task1;
      package Generic2 is
      end Generic2;

      package body Generic2 is
      begin
         Ggi1 := Ggi1 + 1;
         Gga1 := (Gga1 (1) + 1, Gga1 (2) + 1, Gga1 (3) + 1);
         Ggr1 := (D => 1, Field1 => Ggr1.Field1 + 1);
         Ggp1 := new Integer'(Ggp1.all + 1);
         Ggv1 := Pack1.Next (Ggv1);
         Ggt1.Next;
      end Generic2;

      task body Task2 is
      begin
         accept Entry1
           (Ti1 :    out Integer;
            Ta1 :    out Array1;
            Tr1 :    out Record1;
            Tp1 : in out Pointer1;
            Tv1 : in out Pack1.Privy;
            Tt1 : in out Task1)
         do
            Ti1 := Gi1 + 1;
            Ta1 := (Ga1 (1) + 1, Ga1 (2) + 1, Ga1 (3) + 1);
            Tr1 := (D => 1, Field1 => Gr1.Field1 + 1);
            Tp1 := new Integer'(Tp1.all + 1);
            Tv1 := Pack1.Next (Tv1);
            Tt1.Next;
         end Entry1;
      end Task2;

      procedure Proc1
        (Pi1 : in out Integer;
         Pa1 : in out Array1;
         Pr1 : in out Record1;
         Pp1 :    out Pointer1;
         Pv1 :    out Pack1.Privy;
         Pt1 : in out Task1)
      is
      begin
         Pi1 := Pi1 + 1;
         Pa1 := (Pa1 (1) + 1, Pa1 (2) + 1, Pa1 (3) + 1);
         Pr1 := (D => 1, Field1 => Pr1.Field1 + 1);
         Pp1 := new Integer'(Gp1.all + 1);
         Pv1 := Pack1.Next (Gv1);
         Pt1.Next;
      end Proc1;

      package Genpack2 is new Generic2 (Xgi1, Xga1, Xgr1, Xgp1, Xgv1, Xgt1);

   begin
      if Xgi1 /= Ident_Int (1) then
         Failed ("INCORRECT VALUE OF XGI1 (1)");
      end if;

      if Xga1 /= (Ident_Int (1), Ident_Int (1), Ident_Int (1)) then
         Failed ("INCORRECT VALUE OF XGA1 (1)");
      end if;

      if Xgr1 /= (D => 1, Field1 => Ident_Int (1)) then
         Failed ("INCORRECT VALUE OF XGR1 (1)");
      end if;

      if Xgp1 /= Ident (Gp1) or Xgp1.all /= Ident_Int (1) then
         Failed ("INCORRECT VALUE OF XGP1 (1)");
      end if;

      if Pack1."/=" (Xgv1, Pack1.Ident (Pack1.One)) then
         Failed ("INCORRECT VALUE OF XGV1 (1)");
      end if;

      Xgt1.Valu (I);
      if I /= Ident_Int (1) then
         Failed ("INCORRECT RETURN VALUE OF XGT1.VALU (1)");
      end if;

      Proc1 (Xgi1, Xga1, Xgr1, Xgp1, Xgv1, Xgt1);

      if Xgi1 /= Ident_Int (2) then
         Failed ("INCORRECT VALUE OF XGI1 (2)");
      end if;

      if Xga1 /= (Ident_Int (2), Ident_Int (2), Ident_Int (2)) then
         Failed ("INCORRECT VALUE OF XGA1 (2)");
      end if;

      if Xgr1 /= (D => 1, Field1 => Ident_Int (2)) then
         Failed ("INCORRECT VALUE OF XGR1 (2)");
      end if;

      if Xgp1 /= Ident (Gp1) or Xgp1.all /= Ident_Int (2) then
         Failed ("INCORRECT VALUE OF XGP1 (2)");
      end if;

      if Pack1."/=" (Xgv1, Pack1.Ident (Pack1.Two)) then
         Failed ("INCORRECT VALUE OF XGV1 (2)");
      end if;

      Xgt1.Valu (I);
      if I /= Ident_Int (2) then
         Failed ("INCORRECT RETURN VALUE OF XGT1.VALU (2)");
      end if;

      G_Chk_Task.Entry1 (Xgi1, Xga1, Xgr1, Xgp1, Xgv1, Xgt1);

      if Xgi1 /= Ident_Int (3) then
         Failed ("INCORRECT VALUE OF XGI1 (3)");
      end if;

      if Xga1 /= (Ident_Int (3), Ident_Int (3), Ident_Int (3)) then
         Failed ("INCORRECT VALUE OF XGA1 (3)");
      end if;

      if Xgr1 /= (D => 1, Field1 => Ident_Int (3)) then
         Failed ("INCORRECT VALUE OF XGR1 (3)");
      end if;

      if Xgp1 /= Ident (Gp1) or Xgp1.all /= Ident_Int (3) then
         Failed ("INCORRECT VALUE OF XGP1 (3)");
      end if;

      if Pack1."/=" (Xgv1, Pack1.Ident (Pack1.Three)) then
         Failed ("INCORRECT VALUE OF XGV1 (3)");
      end if;

      Xgt1.Valu (I);
      if I /= Ident_Int (3) then
         Failed ("INCORRECT RETURN VALUE OF XGT1.VALU (3)");
      end if;

      Xgi1 := Xgi1 + 1;
      Xga1 := (Xga1 (1) + 1, Xga1 (2) + 1, Xga1 (3) + 1);
      Xgr1 := (D => 1, Field1 => Xgr1.Field1 + 1);
      Xgp1 := new Integer'(Xgp1.all + 1);
      Xgv1 := Pack1.Next (Xgv1);
      Xgt1.Next;

      if Xgi1 /= Ident_Int (4) then
         Failed ("INCORRECT VALUE OF XGI1 (4)");
      end if;

      if Xga1 /= (Ident_Int (4), Ident_Int (4), Ident_Int (4)) then
         Failed ("INCORRECT VALUE OF XGA1 (4)");
      end if;

      if Xgr1 /= (D => 1, Field1 => Ident_Int (4)) then
         Failed ("INCORRECT VALUE OF XGR1 (4)");
      end if;

      if Xgp1 /= Ident (Gp1) or Xgp1.all /= Ident_Int (4) then
         Failed ("INCORRECT VALUE OF XGP1 (4)");
      end if;

      if Pack1."/=" (Xgv1, Pack1.Ident (Pack1.Four)) then
         Failed ("INCORRECT VALUE OF XGV1 (4)");
      end if;

      Xgt1.Valu (I);
      if I /= Ident_Int (4) then
         Failed ("INCORRECT RETURN VALUE OF XGT1.VALU (4)");
      end if;

      Gi1 := Gi1 + 1;
      Ga1 := (Ga1 (1) + 1, Ga1 (2) + 1, Ga1 (3) + 1);
      Gr1 := (D => 1, Field1 => Gr1.Field1 + 1);
      Gp1 := new Integer'(Gp1.all + 1);
      Gv1 := Pack1.Next (Gv1);
      Gt1.Next;

      if Xgi1 /= Ident_Int (5) then
         Failed ("INCORRECT VALUE OF XGI1 (5)");
      end if;

      if Xga1 /= (Ident_Int (5), Ident_Int (5), Ident_Int (5)) then
         Failed ("INCORRECT VALUE OF XGA1 (5)");
      end if;

      if Xgr1 /= (D => 1, Field1 => Ident_Int (5)) then
         Failed ("INCORRECT VALUE OF XGR1 (5)");
      end if;

      if Xgp1 /= Ident (Gp1) or Xgp1.all /= Ident_Int (5) then
         Failed ("INCORRECT VALUE OF XGP1 (5)");
      end if;

      if Pack1."/=" (Xgv1, Pack1.Ident (Pack1.Five)) then
         Failed ("INCORRECT VALUE OF XGV1 (5)");
      end if;

      Xgt1.Valu (I);
      if I /= Ident_Int (5) then
         Failed ("INCORRECT RETURN VALUE OF XGT1.VALU (5)");
      end if;
   end Generic1;

   task body Task1 is
      Task_Value        : Integer := 0;
      Accepting_Entries : Boolean := True;
   begin
      while Accepting_Entries loop
         select
            accept Assign (J : in Integer) do
               Task_Value := J;
            end Assign;
         or
            accept Valu (J : out Integer) do
               J := Task_Value;
            end Valu;
         or
            accept Next do
               Task_Value := Task_Value + 1;
            end Next;
         or
            accept Stop do
               Accepting_Entries := False;
            end Stop;
         end select;
      end loop;
   end Task1;

begin
   Test
     ("C85005D",
      "CHECK THAT A VARIABLE CREATED BY A GENERIC " &
      "'IN OUT' FORMAL PARAMETER CAN BE RENAMED " &
      "AND HAS THE CORRECT VALUE, AND THAT THE NEW " &
      "NAME CAN BE USED IN AN ASSIGNMENT STATEMENT " &
      "AND PASSED ON AS AN ACTUAL SUBPROGRAM OR " &
      "ENTRY 'IN OUT' OR 'OUT' PARAMETER, AND AS AN " &
      "ACTUAL GENERIC 'IN OUT' PARAMETER, AND THAT " &
      "WHEN THE VALUE OF THE RENAMED VARIABLE IS " &
      "CHANGED, THE NEW VALUE IS REFLECTED BY THE " &
      "VALUE OF THE NEW NAME");

   declare
      package Genpack1 is new Generic1 (Di1, Da1, Dr1, Dp1, Dv1, Dt1);
   begin
      null;
   end;

   Dt1.Stop;

   Result;
end C85005d;
