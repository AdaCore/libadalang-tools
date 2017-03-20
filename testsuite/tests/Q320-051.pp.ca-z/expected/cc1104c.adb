-- CC1104C.ADA

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
-- OBJECTIVE;
--     CHECK THAT A GENERIC FORMAL IN OUT PARAMETER CAN HAVE A
--     LIMITED TYPE.

-- HISTORY:
--     BCB 08/03/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure Cc1104c is

   task type Tsk is
      entry E;
   end Tsk;

   Var     : Integer := Ident_Int (0);
   New_Val : Integer := Ident_Int (5);

   Tsk_Var : Tsk;

   package Pp is
      type Lp is limited private;
      procedure Init (One : out Lp; Two : Integer);
      function Equal (One : Lp; Two : Integer) return Boolean;
   private
      type Lp is range 1 .. 100;
   end Pp;

   use Pp;

   type Rec is record
      Comp : Lp;
   end record;

   C : Lp;

   Rec_Var : Rec;

   generic
      type T is limited private;
      In_Out_Var : in out T;
      In_Out_Tsk : in out Tsk;
      Val : in out T;
      with procedure Init (L : in out T; R : T);
   procedure P;

   generic
      Val : in out Lp;
   procedure Q;

   generic
      Val : in out Rec;
   procedure R;

   package body Pp is
      procedure Init (One : out Lp; Two : Integer) is
      begin
         One := Lp (Two);
      end Init;

      function Equal (One : Lp; Two : Integer) return Boolean is
      begin
         return One = Lp (Two);
      end Equal;
   end Pp;

   task body Tsk is
   begin
      accept E;
   end Tsk;

   procedure P is
   begin
      Init (In_Out_Var, Val);
      In_Out_Tsk.E;
      Init (C, 50);
   end P;

   procedure Q is
   begin
      Init (Val, 75);
      Init (Rec_Var.Comp, 50);
   end Q;

   procedure R is
   begin
      Init (Val.Comp, 75);
   end R;

   procedure I (One : in out Integer; Two : Integer) is
   begin
      One := Two;
   end I;

   procedure New_P is new P (Integer, Var, Tsk_Var, New_Val, I);

   procedure New_Q is new Q (C);

   procedure New_R is new R (Rec_Var);

begin
   Test
     ("CC1104C",
      "CHECK THAT A GENERIC FORMAL IN OUT PARAMETER " &
      "CAN HAVE A LIMITED TYPE");

   New_P;

   if not Equal (Var, 5) then
      Failed
        ("WRONG VALUE ASSIGNED TO IN OUT PARAMETER IN " &
         "GENERIC PACKAGE - 1");
   end if;

   New_Q;

   if not Equal (C, 75) then
      Failed
        ("WRONG VALUE ASSIGNED TO IN OUT PARAMETER IN " &
         "GENERIC PACKAGE - 2");
   end if;

   New_R;

   if not Equal (Rec_Var.Comp, 75) then
      Failed
        ("WRONG VALUE ASSIGNED TO IN OUT PARAMETER IN " &
         "GENERIC PACKAGE - 3");
   end if;

   Result;
end Cc1104c;
