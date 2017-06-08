-- C85005B.ADA

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
--     CHECK THAT A VARIABLE CREATED BY A SUBPROGRAM 'IN OUT' FORMAL
--     PARAMETER CAN BE RENAMED AND HAS THE CORRECT VALUE, AND THAT
--     THE NEW NAME CAN BE USED IN AN ASSIGNMENT STATEMENT AND PASSED
--     ON AS AN ACTUAL SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT' PARAMETER,
--     AND AS AN ACTUAL GENERIC 'IN OUT' PARAMETER, AND THAT WHEN THE
--     VALUE OF THE RENAMED VARIABLE IS CHANGED, THE NEW VALUE IS
--     REFLECTED BY THE VALUE OF THE NEW NAME.

-- HISTORY:
--     JET 03/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85005b is

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

   procedure Proc
     (Pi1 : in out Integer;
      Pa1 : in out Array1;
      Pr1 : in out Record1;
      Pp1 : in out Pointer1;
      Pv1 : in out Pack1.Privy;
      Pt1 : in out Task1)
   is
      Xpi1 : Integer renames Pi1;
      Xpa1 : Array1 renames Pa1;
      Xpr1 : Record1 renames Pr1;
      Xpp1 : Pointer1 renames Pp1;
      Xpv1 : Pack1.Privy renames Pv1;
      Xpt1 : Task1 renames Pt1;

      task type Task2 is
         entry Entry1
           (Ti1 :    out Integer;
            Ta1 :    out Array1;
            Tr1 :    out Record1;
            Tp1 : in out Pointer1;
            Tv1 : in out Pack1.Privy;
            Tt1 : in out Task1);
      end Task2;

      Chk_Task : Task2;

      procedure Proc1
        (Ppi1 : in out Integer;
         Ppa1 : in out Array1;
         Ppr1 : in out Record1;
         Ppp1 :    out Pointer1;
         Ppv1 :    out Pack1.Privy;
         Ppt1 : in out Task1)
      is
      begin
         Ppi1 := Ppi1 + 1;
         Ppa1 := (Ppa1 (1) + 1, Ppa1 (2) + 1, Ppa1 (3) + 1);
         Ppr1 := (D => 1, Field1 => Ppr1.Field1 + 1);
         Ppp1 := new Integer'(Pp1.all + 1);
         Ppv1 := Pack1.Next (Pv1);
         Ppt1.Next;
      end Proc1;

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
            Ti1 := Pi1 + 1;
            Ta1 := (Pa1 (1) + 1, Pa1 (2) + 1, Pa1 (3) + 1);
            Tr1 := (D => 1, Field1 => Pr1.Field1 + 1);
            Tp1 := new Integer'(Tp1.all + 1);
            Tv1 := Pack1.Next (Tv1);
            Tt1.Next;
         end Entry1;
      end Task2;

      package Genpack1 is new Generic1 (Xpi1, Xpa1, Xpr1, Xpp1, Xpv1, Xpt1);

   begin
      if Xpi1 /= Ident_Int (1) then
         Failed ("INCORRECT VALUE OF XPI1 (1)");
      end if;

      if Xpa1 /= (Ident_Int (1), Ident_Int (1), Ident_Int (1)) then
         Failed ("INCORRECT VALUE OF XPA1 (1)");
      end if;

      if Xpr1 /= (D => 1, Field1 => Ident_Int (1)) then
         Failed ("INCORRECT VALUE OF XPR1 (1)");
      end if;

      if Xpp1 /= Ident (Pp1) or Xpp1.all /= Ident_Int (1) then
         Failed ("INCORRECT VALUE OF XPP1 (1)");
      end if;

      if Pack1."/=" (Xpv1, Pack1.Ident (Pack1.One)) then
         Failed ("INCORRECT VALUE OF XPV1 (1)");
      end if;

      Xpt1.Valu (I);
      if I /= Ident_Int (1) then
         Failed ("INCORRECT RETURN VALUE OF XPT1.VALU (1)");
      end if;

      Proc1 (Xpi1, Xpa1, Xpr1, Xpp1, Xpv1, Xpt1);

      if Xpi1 /= Ident_Int (2) then
         Failed ("INCORRECT VALUE OF XPI1 (2)");
      end if;

      if Xpa1 /= (Ident_Int (2), Ident_Int (2), Ident_Int (2)) then
         Failed ("INCORRECT VALUE OF XPA1 (2)");
      end if;

      if Xpr1 /= (D => 1, Field1 => Ident_Int (2)) then
         Failed ("INCORRECT VALUE OF XPR1 (2)");
      end if;

      if Xpp1 /= Ident (Pp1) or Xpp1.all /= Ident_Int (2) then
         Failed ("INCORRECT VALUE OF XPP1 (2)");
      end if;

      if Pack1."/=" (Xpv1, Pack1.Ident (Pack1.Two)) then
         Failed ("INCORRECT VALUE OF XPV1 (2)");
      end if;

      Xpt1.Valu (I);
      if I /= Ident_Int (2) then
         Failed ("INCORRECT RETURN VALUE FROM XPT1.VALU (2)");
      end if;

      Chk_Task.Entry1 (Xpi1, Xpa1, Xpr1, Xpp1, Xpv1, Xpt1);

      if Xpi1 /= Ident_Int (3) then
         Failed ("INCORRECT VALUE OF XPI1 (3)");
      end if;

      if Xpa1 /= (Ident_Int (3), Ident_Int (3), Ident_Int (3)) then
         Failed ("INCORRECT VALUE OF XPA1 (3)");
      end if;

      if Xpr1 /= (D => 1, Field1 => Ident_Int (3)) then
         Failed ("INCORRECT VALUE OF XPR1 (3)");
      end if;

      if Xpp1 /= Ident (Pp1) or Xpp1.all /= Ident_Int (3) then
         Failed ("INCORRECT VALUE OF XPP1 (3)");
      end if;

      if Pack1."/=" (Xpv1, Pack1.Ident (Pack1.Three)) then
         Failed ("INCORRECT VALUE OF XPV1 (3)");
      end if;

      Xpt1.Valu (I);
      if I /= Ident_Int (3) then
         Failed ("INCORRECT RETURN VALUE OF XPT1.VALU (3)");
      end if;

      Xpi1 := Xpi1 + 1;
      Xpa1 := (Xpa1 (1) + 1, Xpa1 (2) + 1, Xpa1 (3) + 1);
      Xpr1 := (D => 1, Field1 => Xpr1.Field1 + 1);
      Xpp1 := new Integer'(Xpp1.all + 1);
      Xpv1 := Pack1.Next (Xpv1);
      Xpt1.Next;

      if Xpi1 /= Ident_Int (4) then
         Failed ("INCORRECT VALUE OF XPI1 (4)");
      end if;

      if Xpa1 /= (Ident_Int (4), Ident_Int (4), Ident_Int (4)) then
         Failed ("INCORRECT VALUE OF XPA1 (4)");
      end if;

      if Xpr1 /= (D => 1, Field1 => Ident_Int (4)) then
         Failed ("INCORRECT VALUE OF XPR1 (4)");
      end if;

      if Xpp1 /= Ident (Pp1) or Xpp1.all /= Ident_Int (4) then
         Failed ("INCORRECT VALUE OF XPP1 (4)");
      end if;

      if Pack1."/=" (Xpv1, Pack1.Ident (Pack1.Four)) then
         Failed ("INCORRECT VALUE OF XPV1 (4)");
      end if;

      Xpt1.Valu (I);
      if I /= Ident_Int (4) then
         Failed ("INCORRECT RETURN VALUE OF XPT1.VALU (4)");
      end if;

      Pi1 := Pi1 + 1;
      Pa1 := (Pa1 (1) + 1, Pa1 (2) + 1, Pa1 (3) + 1);
      Pr1 := (D => 1, Field1 => Pr1.Field1 + 1);
      Pp1 := new Integer'(Pp1.all + 1);
      Pv1 := Pack1.Next (Pv1);
      Pt1.Next;

      if Xpi1 /= Ident_Int (5) then
         Failed ("INCORRECT VALUE OF XPI1 (5)");
      end if;

      if Xpa1 /= (Ident_Int (5), Ident_Int (5), Ident_Int (5)) then
         Failed ("INCORRECT VALUE OF XPA1 (5)");
      end if;

      if Xpr1 /= (D => 1, Field1 => Ident_Int (5)) then
         Failed ("INCORRECT VALUE OF XPR1 (5)");
      end if;

      if Xpp1 /= Ident (Pp1) or Xpp1.all /= Ident_Int (5) then
         Failed ("INCORRECT VALUE OF XPP1 (5)");
      end if;

      if Pack1."/=" (Xpv1, Pack1.Ident (Pack1.Five)) then
         Failed ("INCORRECT VALUE OF XPV1 (5)");
      end if;

      Xpt1.Valu (I);
      if I /= Ident_Int (5) then
         Failed ("INCORRECT RETURN VALUE OF XPT1.VALU (5)");
      end if;
   end Proc;

begin
   Test
     ("C85005B",
      "CHECK THAT A VARIABLE CREATED BY A SUBPROGRAM " &
      "'IN OUT' FORMAL PARAMETER CAN BE RENAMED " &
      "AND HAS THE CORRECT VALUE, AND THAT THE NEW " &
      "NAME CAN BE USED IN AN ASSIGNMENT STATEMENT " &
      "AND PASSED ON AS AN ACTUAL SUBPROGRAM OR " &
      "ENTRY 'IN OUT' OR 'OUT' PARAMETER, AND AS AN " &
      "ACTUAL GENERIC 'IN OUT' PARAMETER, AND THAT " &
      "WHEN THE VALUE OF THE RENAMED VARIABLE IS " &
      "CHANGED, THE NEW VALUE IS REFLECTED BY THE " &
      "VALUE OF THE NEW NAME");

   Proc (Di1, Da1, Dr1, Dp1, Dv1, Dt1);

   Dt1.Stop;

   Result;
end C85005b;
