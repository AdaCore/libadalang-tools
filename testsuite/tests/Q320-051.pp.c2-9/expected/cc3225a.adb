-- CC3225A.ADA

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
--     CHECK THAT A FORMAL ACCESS TYPE DENOTES ITS ACTUAL
--     PARAMETER, AND THAT OPERATIONS OF THE FORMAL TYPE ARE THOSE
--     IDENTIFIED WITH THE CORRESPONDING OPERATIONS OF THE ACTUAL TYPE.

-- HISTORY:
--     DHH 10/21/88  CREATED ORIGINAL TEST.
--     PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
procedure Cc3225a is

   generic
      type Node is private;
      type T is access Node;
   package P is
      subtype Sub_T is T;
      Pac_Var : Sub_T;
   end P;

begin
   Test
     ("CC3225A",
      "CHECK THAT A FORMAL ACCESS TYPE DENOTES ITS " &
      "ACTUAL PARAMETER, AND THAT OPERATIONS OF THE " &
      "FORMAL TYPE ARE THOSE IDENTIFIED WITH THE " &
      "CORRESPONDING OPERATIONS OF THE ACTUAL TYPE");

   declare
      subtype Int is Integer range 1 .. 3;
      type Arr is array (1 .. 3) of Integer;
      type Acc_Arr is access Arr;

      Q : Acc_Arr := new Arr;

      package P1 is new P (Arr, Acc_Arr);
      use P1;

   begin
      Pac_Var := new Arr'(1, 2, 3);
      if Pac_Var'First /= Q'First then
         Failed ("'FIRST ATTRIBUTE FAILED");
      end if;
      if Pac_Var'Last /= Q'Last then
         Failed ("'LAST ATTRIBUTE FAILED");
      end if;
      if Pac_Var'First (1) /= Q'First (1) then
         Failed ("'FIRST(N) ATTRIBUTE FAILED");
      end if;
      if not (Pac_Var'Last (1) = Q'Last (1)) then
         Failed ("'LAST(N) ATTRIBUTE FAILED");
      end if;
      if 2 not in Pac_Var'Range then
         Failed ("'RANGE ATTRIBUTE FAILED");
      end if;
      if 3 not in Pac_Var'Range (1) then
         Failed ("'RANGE(N) ATTRIBUTE FAILED");
      end if;
      if Pac_Var'Length /= Q'Length then
         Failed ("'LENGTH ATTRIBUTE FAILED");
      end if;
      if Pac_Var'Length (1) /= Q'Length (1) then
         Failed ("'LENGTH(N) ATTRIBUTE FAILED");
      end if;

      Pac_Var.all := (1, 2, 3);
      if Ident_Int (3) /= Pac_Var (3) then
         Failed ("ASSIGNMENT FAILED");
      end if;

      if Sub_T'(Pac_Var) not in Sub_T then
         Failed ("QUALIFIED EXPRESSION FAILED");
      end if;

      Q.all := Pac_Var.all;
      if Sub_T (Q) = Pac_Var then
         Failed ("EXPLICIT CONVERSION FAILED");
      end if;
      if Q (1) /= Pac_Var (1) then
         Failed ("INDEXING FAILED");
      end if;
      if (1, 2) /= Pac_Var (1 .. 2) then
         Failed ("SLICE FAILED");
      end if;
      if (1, 2) & Pac_Var (3) /= Pac_Var.all then
         Failed ("CATENATION FAILED");
      end if;
   end;

   declare
      task type Tsk is
         entry One;
      end Tsk;

      generic
         type T is access Tsk;
      package P is
         subtype Sub_T is T;
         Pac_Var : Sub_T;
      end P;

      type Acc_Tsk is access Tsk;

      package P1 is new P (Acc_Tsk);
      use P1;

      Global : Integer := 5;

      task body Tsk is
      begin
         accept One do
            Global := 1;
         end One;
      end Tsk;
   begin
      Pac_Var := new Tsk;
      Pac_Var.One;
      if Global /= 1 then
         Failed ("TASK ENTRY SELECTION FAILED");
      end if;
   end;

   declare
      type Rec is record
         I : Integer;
         B : Boolean;
      end record;

      type Acc_Rec is access Rec;

      package P1 is new P (Rec, Acc_Rec);
      use P1;

   begin
      Pac_Var := new Rec'(4, (Pac_Var in Acc_Rec));
      if Pac_Var.I /= Ident_Int (4) and not Pac_Var.B then
         Failed ("RECORD COMPONENT SELECTION FAILED");
      end if;
   end;

   declare
      type Rec (B : Boolean := False) is record
         null;
      end record;

      type Acc_Rec is access Rec;

      package P1 is new P (Rec, Acc_Rec);
      use P1;

   begin
      Pac_Var := new Rec'(B => Pac_Var in Acc_Rec);
      if not Pac_Var.B then
         Failed ("DISCRIMINANT SELECTION FAILED");
      end if;
   end;

   Result;
end Cc3225a;
