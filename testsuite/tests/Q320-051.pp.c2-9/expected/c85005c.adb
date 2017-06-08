-- C85005C.ADA

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
--     CHECK THAT A VARIABLE CREATED BY AN ENTRY 'IN OUT' FORMAL
--     PARAMETER CAN BE RENAMED AND HAS THE CORRECT VALUE, AND THAT
--     THE NEW NAME CAN BE USED IN AN ASSIGNMENT STATEMENT AND PASSED
--     ON AS AN ACTUAL SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT' PARAMETER,
--     AND AS AN ACTUAL GENERIC 'IN OUT' PARAMETER, AND THAT WHEN THE
--     VALUE OF THE RENAMED VARIABLE IS CHANGED, THE NEW VALUE IS
--     REFLECTED BY THE VALUE OF THE NEW NAME.

-- HISTORY:
--     JET 03/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85005c is

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
   begin
      Gi1 := Gi1 + 1;
      Ga1 := (Ga1 (1) + 1, Ga1 (2) + 1, Ga1 (3) + 1);
      Gr1 := (D => 1, Field1 => Gr1.Field1 + 1);
      Gp1 := new Integer'(Gp1.all + 1);
      Gv1 := Pack1.Next (Gv1);
      Gt1.Next;
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
     ("C85005C",
      "CHECK THAT A VARIABLE CREATED BY AN ENTRY " &
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
      task Main_Task is
         entry Start
           (Ti1 : in out Integer;
            Ta1 : in out Array1;
            Tr1 : in out Record1;
            Tp1 : in out Pointer1;
            Tv1 : in out Pack1.Privy;
            Tt1 : in out Task1);
      end Main_Task;

      task body Main_Task is
      begin
         accept Start
           (Ti1 : in out Integer;
            Ta1 : in out Array1;
            Tr1 : in out Record1;
            Tp1 : in out Pointer1;
            Tv1 : in out Pack1.Privy;
            Tt1 : in out Task1)
         do
            declare
               Xti1 : Integer renames Ti1;
               Xta1 : Array1 renames Ta1;
               Xtr1 : Record1 renames Tr1;
               Xtp1 : Pointer1 renames Tp1;
               Xtv1 : Pack1.Privy renames Tv1;
               Xtt1 : Task1 renames Tt1;

               task type Task2 is
                  entry Entry1
                    (Tti1 :    out Integer;
                     Tta1 :    out Array1;
                     Ttr1 :    out Record1;
                     Ttp1 : in out Pointer1;
                     Ttv1 : in out Pack1.Privy;
                     Ttt1 : in out Task1);
               end Task2;

               Chk_Task : Task2;

               procedure Proc1
                 (Pti1 : in out Integer;
                  Pta1 : in out Array1;
                  Ptr1 : in out Record1;
                  Ptp1 :    out Pointer1;
                  Ptv1 :    out Pack1.Privy;
                  Ptt1 : in out Task1)
               is
               begin
                  Pti1 := Pti1 + 1;
                  Pta1 := (Pta1 (1) + 1, Pta1 (2) + 1, Pta1 (3) + 1);
                  Ptr1 := (D => 1, Field1 => Ptr1.Field1 + 1);
                  Ptp1 := new Integer'(Tp1.all + 1);
                  Ptv1 := Pack1.Next (Tv1);
                  Ptt1.Next;
               end Proc1;

               task body Task2 is
               begin
                  accept Entry1
                    (Tti1 :    out Integer;
                     Tta1 :    out Array1;
                     Ttr1 :    out Record1;
                     Ttp1 : in out Pointer1;
                     Ttv1 : in out Pack1.Privy;
                     Ttt1 : in out Task1)
                  do
                     Tti1 := Ti1 + 1;
                     Tta1 := (Ta1 (1) + 1, Ta1 (2) + 1, Ta1 (3) + 1);
                     Ttr1 := (D => 1, Field1 => Tr1.Field1 + 1);
                     Ttp1 := new Integer'(Ttp1.all + 1);
                     Ttv1 := Pack1.Next (Ttv1);
                     Ttt1.Next;
                  end Entry1;
               end Task2;

               package Genpack1 is new Generic1
                 (Xti1,
                  Xta1,
                  Xtr1,
                  Xtp1,
                  Xtv1,
                  Xtt1);
            begin
               if Xti1 /= Ident_Int (1) then
                  Failed ("INCORRECT VALUE OF XTI1 (1)");
               end if;

               if Xta1 /= (Ident_Int (1), Ident_Int (1), Ident_Int (1)) then
                  Failed ("INCORRECT VALUE OF XTA1 (1)");
               end if;

               if Xtr1 /= (D => 1, Field1 => Ident_Int (1)) then
                  Failed ("INCORRECT VALUE OF XTR1 (1)");
               end if;

               if Xtp1 /= Ident (Tp1) or Xtp1.all /= Ident_Int (1) then
                  Failed ("INCORRECT VALUE OF XTP1 (1)");
               end if;

               if Pack1."/=" (Xtv1, Pack1.Ident (Pack1.One)) then
                  Failed ("INCORRECT VALUE OF XTV1 (1)");
               end if;

               Xtt1.Valu (I);
               if I /= Ident_Int (1) then
                  Failed ("INCORRECT RETURN VALUE OF " & "XTT1.VALU (1)");
               end if;

               Proc1 (Xti1, Xta1, Xtr1, Xtp1, Xtv1, Xtt1);

               if Xti1 /= Ident_Int (2) then
                  Failed ("INCORRECT VALUE OF XTI1 (2)");
               end if;

               if Xta1 /= (Ident_Int (2), Ident_Int (2), Ident_Int (2)) then
                  Failed ("INCORRECT VALUE OF XTA1 (2)");
               end if;

               if Xtr1 /= (D => 1, Field1 => Ident_Int (2)) then
                  Failed ("INCORRECT VALUE OF XTR1 (2)");
               end if;

               if Xtp1 /= Ident (Tp1) or Xtp1.all /= Ident_Int (2) then
                  Failed ("INCORRECT VALUE OF XTP1 (2)");
               end if;

               if Pack1."/=" (Xtv1, Pack1.Ident (Pack1.Two)) then
                  Failed ("INCORRECT VALUE OF XTV1 (2)");
               end if;

               Xtt1.Valu (I);
               if I /= Ident_Int (2) then
                  Failed ("INCORRECT RETURN VALUE FROM " & "XTT1.VALU (2)");
               end if;

               Chk_Task.Entry1 (Xti1, Xta1, Xtr1, Xtp1, Xtv1, Xtt1);

               if Xti1 /= Ident_Int (3) then
                  Failed ("INCORRECT VALUE OF XTI1 (3)");
               end if;

               if Xta1 /= (Ident_Int (3), Ident_Int (3), Ident_Int (3)) then
                  Failed ("INCORRECT VALUE OF XTA1 (3)");
               end if;

               if Xtr1 /= (D => 1, Field1 => Ident_Int (3)) then
                  Failed ("INCORRECT VALUE OF XTR1 (3)");
               end if;

               if Xtp1 /= Ident (Tp1) or Xtp1.all /= Ident_Int (3) then
                  Failed ("INCORRECT VALUE OF XTP1 (3)");
               end if;

               if Pack1."/=" (Xtv1, Pack1.Ident (Pack1.Three)) then
                  Failed ("INCORRECT VALUE OF XTV1 (3)");
               end if;

               Xtt1.Valu (I);
               if I /= Ident_Int (3) then
                  Failed ("INCORRECT RETURN VALUE OF " & "XTT1.VALU (3)");
               end if;

               Xti1 := Xti1 + 1;
               Xta1 := (Xta1 (1) + 1, Xta1 (2) + 1, Xta1 (3) + 1);
               Xtr1 := (D => 1, Field1 => Xtr1.Field1 + 1);
               Xtp1 := new Integer'(Xtp1.all + 1);
               Xtv1 := Pack1.Next (Xtv1);
               Xtt1.Next;

               if Xti1 /= Ident_Int (4) then
                  Failed ("INCORRECT VALUE OF XTI1 (4)");
               end if;

               if Xta1 /= (Ident_Int (4), Ident_Int (4), Ident_Int (4)) then
                  Failed ("INCORRECT VALUE OF XTA1 (4)");
               end if;

               if Xtr1 /= (D => 1, Field1 => Ident_Int (4)) then
                  Failed ("INCORRECT VALUE OF XTR1 (4)");
               end if;

               if Xtp1 /= Ident (Tp1) or Xtp1.all /= Ident_Int (4) then
                  Failed ("INCORRECT VALUE OF XTP1 (4)");
               end if;

               if Pack1."/=" (Xtv1, Pack1.Ident (Pack1.Four)) then
                  Failed ("INCORRECT VALUE OF XTV1 (4)");
               end if;

               Xtt1.Valu (I);
               if I /= Ident_Int (4) then
                  Failed ("INCORRECT RETURN VALUE OF " & "XTT1.VALU (4)");
               end if;

               Ti1 := Ti1 + 1;
               Ta1 := (Ta1 (1) + 1, Ta1 (2) + 1, Ta1 (3) + 1);
               Tr1 := (D => 1, Field1 => Tr1.Field1 + 1);
               Tp1 := new Integer'(Tp1.all + 1);
               Tv1 := Pack1.Next (Tv1);
               Tt1.Next;

               if Xti1 /= Ident_Int (5) then
                  Failed ("INCORRECT VALUE OF XTI1 (5)");
               end if;

               if Xta1 /= (Ident_Int (5), Ident_Int (5), Ident_Int (5)) then
                  Failed ("INCORRECT VALUE OF XTA1 (5)");
               end if;

               if Xtr1 /= (D => 1, Field1 => Ident_Int (5)) then
                  Failed ("INCORRECT VALUE OF XTR1 (5)");
               end if;

               if Xtp1 /= Ident (Tp1) or Xtp1.all /= Ident_Int (5) then
                  Failed ("INCORRECT VALUE OF XTP1 (5)");
               end if;

               if Pack1."/=" (Xtv1, Pack1.Ident (Pack1.Five)) then
                  Failed ("INCORRECT VALUE OF XTV1 (5)");
               end if;

               Xtt1.Valu (I);
               if I /= Ident_Int (5) then
                  Failed ("INCORRECT RETURN VALUE OF " & "XTT1.VALU (5)");
               end if;
            end;
         end Start;
      end Main_Task;

   begin
      Main_Task.Start (Di1, Da1, Dr1, Dp1, Dv1, Dt1);
   end;

   Dt1.Stop;

   Result;
end C85005c;
