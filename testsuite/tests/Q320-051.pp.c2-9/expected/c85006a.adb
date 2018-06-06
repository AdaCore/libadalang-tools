-- C85006A.ADA

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
--     CHECK THAT A COMPONENT OR SLICE OF A VARIABLE CREATED BY AN
--     OBJECT DECLARATION CAN BE RENAMED AND HAS THE CORRECT VALUE,
--     AND THAT THE NEW NAME CAN BE USED IN AN ASSIGNMENT STATEMENT
--     AND PASSED ON AS AN ACTUAL SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT'
--     PARAMETER, AND AS AN ACTUAL GENERIC 'IN OUT' PARAMETER,
--     AND THAT WHEN THE VALUE OF THE RENAMED VARIABLE IS CHANGED,
--     THE NEW VALUE IS REFLECTED BY THE VALUE OF THE NEW NAME.

-- HISTORY:
--     JET 03/22/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85006a is

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

   type Arr_Int is array (Positive range <>) of Integer;
   type Arr_Arr is array (Positive range <>) of Array1 (1 .. 3);
   type Arr_Rec is array (Positive range <>) of Record1 (1);
   type Arr_Ptr is array (Positive range <>) of Pointer1;
   type Arr_Pvt is array (Positive range <>) of Pack1.Privy;
   type Arr_Tsk is array (Positive range <>) of Task1;

   task type Task2 is
      entry Entry1
        (Tri1 :    out Integer; Tra1 : out Array1; Trr1 : out Record1;
         Trp1 : in out Pointer1; Trv1 : in out Pack1.Privy;
         Trt1 : in out Task1; Tai1 : out Arr_Int; Taa1 : out Arr_Arr;
         Tar1 :    out Arr_Rec; Tap1 : in out Arr_Ptr; Tav1 : in out Arr_Pvt;
         Tat1 : in out Arr_Tsk);
   end Task2;

   type Rec_Type is record
      Ri1 : Integer         := 0;
      Ra1 : Array1 (1 .. 3) := (others => 0);
      Rr1 : Record1 (1)     := (D => 1, Field1 => 0);
      Rp1 : Pointer1        := new Integer'(0);
      Rv1 : Pack1.Privy     := Pack1.Zero;
      Rt1 : Task1;
   end record;

   Rec : Rec_Type;

   Ai1 : Arr_Int (1 .. 8) := (others => 0);
   Aa1 : Arr_Arr (1 .. 8) := (others => (others => 0));
   Ar1 : Arr_Rec (1 .. 8) := (others => (D => 1, Field1 => 0));
   Ap1 : Arr_Ptr (1 .. 8) := (others => new Integer'(0));
   Av1 : Arr_Pvt (1 .. 8) := (others => Pack1.Zero);
   At1 : Arr_Tsk (1 .. 8);

   Xri1 : Integer renames Rec.Ri1;
   Xra1 : Array1 renames Rec.Ra1;
   Xrr1 : Record1 renames Rec.Rr1;
   Xrp1 : Pointer1 renames Rec.Rp1;
   Xrv1 : Pack1.Privy renames Rec.Rv1;
   Xrt1 : Task1 renames Rec.Rt1;
   Xai1 : Arr_Int renames Ai1 (1 .. 3);
   Xaa1 : Arr_Arr renames Aa1 (2 .. 4);
   Xar1 : Arr_Rec renames Ar1 (3 .. 5);
   Xap1 : Arr_Ptr renames Ap1 (4 .. 6);
   Xav1 : Arr_Pvt renames Av1 (5 .. 7);
   Xat1 : Arr_Tsk renames At1 (6 .. 8);

   I        : Integer;
   Chk_Task : Task2;

   generic
      Gri1 : in out Integer;
      Gra1 : in out Array1;
      Grr1 : in out Record1;
      Grp1 : in out Pointer1;
      Grv1 : in out Pack1.Privy;
      Grt1 : in out Task1;
      Gai1 : in out Arr_Int;
      Gaa1 : in out Arr_Arr;
      Gar1 : in out Arr_Rec;
      Gap1 : in out Arr_Ptr;
      Gav1 : in out Arr_Pvt;
      Gat1 : in out Arr_Tsk;
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

   procedure Proc1
     (Pri1 : in out Integer; Pra1 : in out Array1; Prr1 : in out Record1;
      Prp1 :    out Pointer1; Prv1 : out Pack1.Privy; Prt1 : in out Task1;
      Pai1 : in out Arr_Int; Paa1 : in out Arr_Arr; Par1 : in out Arr_Rec;
      Pap1 :    out Arr_Ptr; Pav1 : out Arr_Pvt; Pat1 : in out Arr_Tsk)
   is

   begin
      Pri1 := Pri1 + 1;
      Pra1 := (Pra1 (1) + 1, Pra1 (2) + 1, Pra1 (3) + 1);
      Prr1 := (D => 1, Field1 => Prr1.Field1 + 1);
      Prp1 := new Integer'(Rec.Rp1.all + 1);
      Prv1 := Pack1.Next (Rec.Rv1);
      Prt1.Next;
      Pai1 := (others => Pai1 (Pai1'First) + 1);
      Paa1 := (others => (others => Paa1 (Paa1'First) (1) + 1));
      Par1 := (others => (D => 1, Field1 => (Par1 (Par1'First).Field1 + 1)));
      Pap1 := (others => new Integer'(Ap1 (Pap1'First).all + 1));
      for J in Pav1'Range loop
         Pav1 (J) := Pack1.Next (Av1 (J));
      end loop;
      for J in Pat1'Range loop
         Pat1 (J).Next;
      end loop;
   end Proc1;

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
      Gri1 := Gri1 + 1;
      Gra1 := (Gra1 (1) + 1, Gra1 (2) + 1, Gra1 (3) + 1);
      Grr1 := (D => 1, Field1 => Grr1.Field1 + 1);
      Grp1 := new Integer'(Grp1.all + 1);
      Grv1 := Pack1.Next (Grv1);
      Grt1.Next;
      Gai1 := (others => Gai1 (Gai1'First) + 1);
      Gaa1 := (others => (others => Gaa1 (Gaa1'First) (1) + 1));
      Gar1 := (others => (D => 1, Field1 => (Gar1 (Gar1'First).Field1 + 1)));
      Gap1 := (others => new Integer'(Gap1 (Gap1'First).all + 1));
      for J in Gav1'Range loop
         Gav1 (J) := Pack1.Next (Gav1 (J));
      end loop;
      for J in Gat1'Range loop
         Gat1 (J).Next;
      end loop;
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

   task body Task2 is
   begin
      accept Entry1
        (Tri1 :    out Integer; Tra1 : out Array1; Trr1 : out Record1;
         Trp1 : in out Pointer1; Trv1 : in out Pack1.Privy;
         Trt1 : in out Task1; Tai1 : out Arr_Int; Taa1 : out Arr_Arr;
         Tar1 :    out Arr_Rec; Tap1 : in out Arr_Ptr; Tav1 : in out Arr_Pvt;
         Tat1 : in out Arr_Tsk)
      do
         Tri1 := Rec.Ri1 + 1;
         Tra1 := (Rec.Ra1 (1) + 1, Rec.Ra1 (2) + 1, Rec.Ra1 (3) + 1);
         Trr1 := (D => 1, Field1 => Rec.Rr1.Field1 + 1);
         Trp1 := new Integer'(Trp1.all + 1);
         Trv1 := Pack1.Next (Trv1);
         Trt1.Next;
         Tai1 := (others => Ai1 (Tai1'First) + 1);
         Taa1 := (others => (others => Aa1 (Taa1'First) (1) + 1));
         Tar1 := (others => (D => 1, Field1 => (Ar1 (Tar1'First).Field1 + 1)));
         Tap1 := (others => new Integer'(Tap1 (Tap1'First).all + 1));
         for J in Tav1'Range loop
            Tav1 (J) := Pack1.Next (Tav1 (J));
         end loop;
         for J in Tat1'Range loop
            Tat1 (J).Next;
         end loop;
      end Entry1;
   end Task2;

begin
   Test
     ("C85006A",
      "CHECK THAT A COMPONENT OR SLICE OF A VARIABLE " &
      "CREATED BY AN OBJECT DECLARATION CAN BE " &
      "RENAMED AND HAS THE CORRECT VALUE, AND THAT " &
      "THE NEW NAME CAN BE USED IN AN ASSIGNMENT " &
      "STATEMENT AND PASSED ON AS AN ACTUAL " &
      "SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT' " &
      "PARAMETER, AND AS AN ACTUAL GENERIC 'IN OUT' " &
      "PARAMETER, AND THAT WHEN THE VALUE OF THE " &
      "RENAMED VARIABLE IS CHANGED, THE NEW VALUE IS " &
      "REFLECTED BY THE VALUE OF THE NEW NAME");

   declare
      package Genpack1 is new Generic1 (Xri1, Xra1, Xrr1, Xrp1, Xrv1, Xrt1,
         Xai1, Xaa1, Xar1, Xap1, Xav1, Xat1);
   begin
      null;
   end;

   if Xri1 /= Ident_Int (1) then
      Failed ("INCORRECT VALUE OF XRI1 (1)");
   end if;

   if Xra1 /= (Ident_Int (1), Ident_Int (1), Ident_Int (1)) then
      Failed ("INCORRECT VALUE OF XRA1 (1)");
   end if;

   if Xrr1 /= (D => 1, Field1 => Ident_Int (1)) then
      Failed ("INCORRECT VALUE OF XRR1 (1)");
   end if;

   if Xrp1 /= Ident (Rec.Rp1) or Xrp1.all /= Ident_Int (1) then
      Failed ("INCORRECT VALUE OF XRP1 (1)");
   end if;

   if Pack1."/=" (Xrv1, Pack1.Ident (Pack1.One)) then
      Failed ("INCORRECT VALUE OF XRV1 (1)");
   end if;

   Xrt1.Valu (I);
   if I /= Ident_Int (1) then
      Failed ("INCORRECT RETURN VALUE OF XRT1.VALU (1)");
   end if;

   for J in Xai1'Range loop
      if Xai1 (J) /= Ident_Int (1) then
         Failed ("INCORRECT VALUE OF XAI1(" & Integer'Image (J) & ") (1)");
      end if;
   end loop;

   for J in Xaa1'Range loop
      if Xaa1 (J) /= (Ident_Int (1), Ident_Int (1), Ident_Int (1)) then
         Failed ("INCORRECT VALUE OF XAA1(" & Integer'Image (J) & ") (1)");
      end if;
   end loop;

   for J in Xar1'Range loop
      if Xar1 (J) /= (D => 1, Field1 => Ident_Int (1)) then
         Failed ("INCORRECT VALUE OF XAR1(" & Integer'Image (J) & ") (1)");
      end if;
   end loop;

   for J in Xap1'Range loop
      if Xap1 (J) /= Ident (Ap1 (J)) or Xap1 (J).all /= Ident_Int (1) then
         Failed ("INCORRECT VALUE OF XAP1(" & Integer'Image (J) & ") (1)");
      end if;
   end loop;

   for J in Xav1'Range loop
      if Pack1."/=" (Xav1 (J), Pack1.Ident (Pack1.One)) then
         Failed ("INCORRECT VALUE OF XAV1(" & Integer'Image (J) & ") (1)");
      end if;
   end loop;

   for J in Xat1'Range loop
      Xat1 (J).Valu (I);
      if I /= Ident_Int (1) then
         Failed
           ("INCORRECT RETURN VALUE FROM XAT1(" & Integer'Image (J) &
            ").VALU (1)");
      end if;
   end loop;

   Proc1
     (Xri1, Xra1, Xrr1, Xrp1, Xrv1, Xrt1, Xai1, Xaa1, Xar1, Xap1, Xav1, Xat1);

   if Xri1 /= Ident_Int (2) then
      Failed ("INCORRECT VALUE OF XRI1 (2)");
   end if;

   if Xra1 /= (Ident_Int (2), Ident_Int (2), Ident_Int (2)) then
      Failed ("INCORRECT VALUE OF XRA1 (2)");
   end if;

   if Xrr1 /= (D => 1, Field1 => Ident_Int (2)) then
      Failed ("INCORRECT VALUE OF XRR1 (2)");
   end if;

   if Xrp1 /= Ident (Rec.Rp1) or Xrp1.all /= Ident_Int (2) then
      Failed ("INCORRECT VALUE OF XRP1 (2)");
   end if;

   if Pack1."/=" (Xrv1, Pack1.Ident (Pack1.Two)) then
      Failed ("INCORRECT VALUE OF XRV1 (2)");
   end if;

   Xrt1.Valu (I);
   if I /= Ident_Int (2) then
      Failed ("INCORRECT RETURN VALUE FROM XRT1.VALU (2)");
   end if;

   for J in Xai1'Range loop
      if Xai1 (J) /= Ident_Int (2) then
         Failed ("INCORRECT VALUE OF XAI1(" & Integer'Image (J) & ") (2)");
      end if;
   end loop;

   for J in Xaa1'Range loop
      if Xaa1 (J) /= (Ident_Int (2), Ident_Int (2), Ident_Int (2)) then
         Failed ("INCORRECT VALUE OF XAA1(" & Integer'Image (J) & ") (2)");
      end if;
   end loop;

   for J in Xar1'Range loop
      if Xar1 (J) /= (D => 1, Field1 => Ident_Int (2)) then
         Failed ("INCORRECT VALUE OF XAR1(" & Integer'Image (J) & ") (2)");
      end if;
   end loop;

   for J in Xap1'Range loop
      if Xap1 (J) /= Ident (Ap1 (J)) or Xap1 (J).all /= Ident_Int (2) then
         Failed ("INCORRECT VALUE OF XAP1(" & Integer'Image (J) & ") (2)");
      end if;
   end loop;

   for J in Xav1'Range loop
      if Pack1."/=" (Xav1 (J), Pack1.Ident (Pack1.Two)) then
         Failed ("INCORRECT VALUE OF XAV1(" & Integer'Image (J) & ") (2)");
      end if;
   end loop;

   for J in Xat1'Range loop
      Xat1 (J).Valu (I);
      if I /= Ident_Int (2) then
         Failed
           ("INCORRECT RETURN VALUE FROM XAT1(" & Integer'Image (J) &
            ").VALU (2)");
      end if;
   end loop;

   Chk_Task.Entry1
     (Xri1, Xra1, Xrr1, Xrp1, Xrv1, Xrt1, Xai1, Xaa1, Xar1, Xap1, Xav1, Xat1);

   if Xri1 /= Ident_Int (3) then
      Failed ("INCORRECT VALUE OF XRI1 (3)");
   end if;

   if Xra1 /= (Ident_Int (3), Ident_Int (3), Ident_Int (3)) then
      Failed ("INCORRECT VALUE OF XRA1 (3)");
   end if;

   if Xrr1 /= (D => 1, Field1 => Ident_Int (3)) then
      Failed ("INCORRECT VALUE OF XRR1 (3)");
   end if;

   if Xrp1 /= Ident (Rec.Rp1) or Xrp1.all /= Ident_Int (3) then
      Failed ("INCORRECT VALUE OF XRP1 (3)");
   end if;

   if Pack1."/=" (Xrv1, Pack1.Ident (Pack1.Three)) then
      Failed ("INCORRECT VALUE OF XRV1 (3)");
   end if;

   Xrt1.Valu (I);
   if I /= Ident_Int (3) then
      Failed ("INCORRECT RETURN VALUE OF XRT1.VALU (3)");
   end if;

   for J in Xai1'Range loop
      if Xai1 (J) /= Ident_Int (3) then
         Failed ("INCORRECT VALUE OF XAI1(" & Integer'Image (J) & ") (3)");
      end if;
   end loop;

   for J in Xaa1'Range loop
      if Xaa1 (J) /= (Ident_Int (3), Ident_Int (3), Ident_Int (3)) then
         Failed ("INCORRECT VALUE OF XAA1(" & Integer'Image (J) & ") (3)");
      end if;
   end loop;

   for J in Xar1'Range loop
      if Xar1 (J) /= (D => 1, Field1 => Ident_Int (3)) then
         Failed ("INCORRECT VALUE OF XAR1(" & Integer'Image (J) & ") (3)");
      end if;
   end loop;

   for J in Xap1'Range loop
      if Xap1 (J) /= Ident (Ap1 (J)) or Xap1 (J).all /= Ident_Int (3) then
         Failed ("INCORRECT VALUE OF XAP1(" & Integer'Image (J) & ") (3)");
      end if;
   end loop;

   for J in Xav1'Range loop
      if Pack1."/=" (Xav1 (J), Pack1.Ident (Pack1.Three)) then
         Failed ("INCORRECT VALUE OF XAV1(" & Integer'Image (J) & ") (3)");
      end if;
   end loop;

   for J in Xat1'Range loop
      Xat1 (J).Valu (I);
      if I /= Ident_Int (3) then
         Failed
           ("INCORRECT RETURN VALUE FROM XAT1(" & Integer'Image (J) &
            ").VALU (3)");
      end if;
   end loop;

   Xri1 := Xri1 + 1;
   Xra1 := (Xra1 (1) + 1, Xra1 (2) + 1, Xra1 (3) + 1);
   Xrr1 := (D => 1, Field1 => Xrr1.Field1 + 1);
   Xrp1 := new Integer'(Xrp1.all + 1);
   Xrv1 := Pack1.Next (Xrv1);
   Xrt1.Next;
   Xai1 := (others => Xai1 (Xai1'First) + 1);
   Xaa1 := (others => (others => Xaa1 (Xaa1'First) (1) + 1));
   Xar1 := (others => (D => 1, Field1 => (Xar1 (Xar1'First).Field1 + 1)));
   Xap1 := (others => new Integer'(Xap1 (Xap1'First).all + 1));
   for J in Xav1'Range loop
      Xav1 (J) := Pack1.Next (Xav1 (J));
   end loop;
   for J in Xat1'Range loop
      Xat1 (J).Next;
   end loop;

   if Xri1 /= Ident_Int (4) then
      Failed ("INCORRECT VALUE OF XRI1 (4)");
   end if;

   if Xra1 /= (Ident_Int (4), Ident_Int (4), Ident_Int (4)) then
      Failed ("INCORRECT VALUE OF XRA1 (4)");
   end if;

   if Xrr1 /= (D => 1, Field1 => Ident_Int (4)) then
      Failed ("INCORRECT VALUE OF XRR1 (4)");
   end if;

   if Xrp1 /= Ident (Rec.Rp1) or Xrp1.all /= Ident_Int (4) then
      Failed ("INCORRECT VALUE OF XRP1 (4)");
   end if;

   if Pack1."/=" (Xrv1, Pack1.Ident (Pack1.Four)) then
      Failed ("INCORRECT VALUE OF XRV1 (4)");
   end if;

   Xrt1.Valu (I);
   if I /= Ident_Int (4) then
      Failed ("INCORRECT RETURN VALUE OF XRT1.VALU (4)");
   end if;

   for J in Xai1'Range loop
      if Xai1 (J) /= Ident_Int (4) then
         Failed ("INCORRECT VALUE OF XAI1(" & Integer'Image (J) & ") (4)");
      end if;
   end loop;

   for J in Xaa1'Range loop
      if Xaa1 (J) /= (Ident_Int (4), Ident_Int (4), Ident_Int (4)) then
         Failed ("INCORRECT VALUE OF XAA1(" & Integer'Image (J) & ") (4)");
      end if;
   end loop;

   for J in Xar1'Range loop
      if Xar1 (J) /= (D => 1, Field1 => Ident_Int (4)) then
         Failed ("INCORRECT VALUE OF XAR1(" & Integer'Image (J) & ") (4)");
      end if;
   end loop;

   for J in Xap1'Range loop
      if Xap1 (J) /= Ident (Ap1 (J)) or Xap1 (J).all /= Ident_Int (4) then
         Failed ("INCORRECT VALUE OF XAP1(" & Integer'Image (J) & ") (4)");
      end if;
   end loop;

   for J in Xav1'Range loop
      if Pack1."/=" (Xav1 (J), Pack1.Ident (Pack1.Four)) then
         Failed ("INCORRECT VALUE OF XAV1(" & Integer'Image (J) & ") (4)");
      end if;
   end loop;

   for J in Xat1'Range loop
      Xat1 (J).Valu (I);
      if I /= Ident_Int (4) then
         Failed
           ("INCORRECT RETURN VALUE FROM XAT1(" & Integer'Image (J) &
            ").VALU (4)");
      end if;
   end loop;

   Rec.Ri1 := Rec.Ri1 + 1;
   Rec.Ra1 := (Rec.Ra1 (1) + 1, Rec.Ra1 (2) + 1, Rec.Ra1 (3) + 1);
   Rec.Rr1 := (D => 1, Field1 => Rec.Rr1.Field1 + 1);
   Rec.Rp1 := new Integer'(Rec.Rp1.all + 1);
   Rec.Rv1 := Pack1.Next (Rec.Rv1);
   Rec.Rt1.Next;
   Ai1 := (others => Ai1 (Xai1'First) + 1);
   Aa1 := (others => (others => Aa1 (Xaa1'First) (1) + 1));
   Ar1 := (others => (D => 1, Field1 => (Ar1 (Xar1'First).Field1 + 1)));
   Ap1 := (others => new Integer'(Ap1 (Xap1'First).all + 1));
   for J in Xav1'Range loop
      Av1 (J) := Pack1.Next (Av1 (J));
   end loop;
   for J in Xat1'Range loop
      At1 (J).Next;
   end loop;

   if Xri1 /= Ident_Int (5) then
      Failed ("INCORRECT VALUE OF XRI1 (5)");
   end if;

   if Xra1 /= (Ident_Int (5), Ident_Int (5), Ident_Int (5)) then
      Failed ("INCORRECT VALUE OF XRA1 (5)");
   end if;

   if Xrr1 /= (D => 1, Field1 => Ident_Int (5)) then
      Failed ("INCORRECT VALUE OF XRR1 (5)");
   end if;

   if Xrp1 /= Ident (Rec.Rp1) or Xrp1.all /= Ident_Int (5) then
      Failed ("INCORRECT VALUE OF XRP1 (5)");
   end if;

   if Pack1."/=" (Xrv1, Pack1.Ident (Pack1.Five)) then
      Failed ("INCORRECT VALUE OF XRV1 (5)");
   end if;

   Xrt1.Valu (I);
   if I /= Ident_Int (5) then
      Failed ("INCORRECT RETURN VALUE OF XRT1.VALU (5)");
   end if;

   for J in Xai1'Range loop
      if Xai1 (J) /= Ident_Int (5) then
         Failed ("INCORRECT VALUE OF XAI1(" & Integer'Image (J) & ") (5)");
      end if;
   end loop;

   for J in Xaa1'Range loop
      if Xaa1 (J) /= (Ident_Int (5), Ident_Int (5), Ident_Int (5)) then
         Failed ("INCORRECT VALUE OF XAA1(" & Integer'Image (J) & ") (5)");
      end if;
   end loop;

   for J in Xar1'Range loop
      if Xar1 (J) /= (D => 1, Field1 => Ident_Int (5)) then
         Failed ("INCORRECT VALUE OF XAR1(" & Integer'Image (J) & ") (5)");
      end if;
   end loop;

   for J in Xap1'Range loop
      if Xap1 (J) /= Ident (Ap1 (J)) or Xap1 (J).all /= Ident_Int (5) then
         Failed ("INCORRECT VALUE OF XAP1(" & Integer'Image (J) & ") (5)");
      end if;
   end loop;

   for J in Xav1'Range loop
      if Pack1."/=" (Xav1 (J), Pack1.Ident (Pack1.Five)) then
         Failed ("INCORRECT VALUE OF XAV1(" & Integer'Image (J) & ") (5)");
      end if;
   end loop;

   for J in Xat1'Range loop
      Xat1 (J).Valu (I);
      if I /= Ident_Int (5) then
         Failed
           ("INCORRECT RETURN VALUE FROM XAT1(" & Integer'Image (J) &
            ").VALU (5)");
      end if;
   end loop;

   Rec.Rt1.Stop;

   for I in At1'Range loop
      At1 (I).Stop;
   end loop;

   Result;
end C85006a;
