-- C45291A.ADA

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
--     CHECK THAT THE MEMBERSHIP TESTS YIELD CORRECT RESULTS FOR TASK
--     TYPES, LIMITED PRIVATE TYPES, COMPOSITE LIMITED TYPES, AND
--     PRIVATE TYPES WITHOUT DISCRIMINANTS.

-- HISTORY:
--     JET 08/10/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C45291a is

   task type Task1 is
      entry E;
   end Task1;

   package Pack is
      type Lim_Priv is limited private;
      type Lim_Comp is array (1 .. 10) of Lim_Priv;
      type Priv is private;
      procedure Init (Lp : out Lim_Priv; Lc : in out Lim_Comp; P : out Priv);
   private
      type Lim_Priv is range -100 .. 100;
      type Priv is record
         I : Integer;
      end record;
   end Pack;

   subtype Sub_Task1 is Task1;
   subtype Sub_Lim_Priv is Pack.Lim_Priv;
   subtype Sub_Lim_Comp is Pack.Lim_Comp;
   subtype Sub_Priv is Pack.Priv;

   T1 : Task1;
   Lp : Pack.Lim_Priv;
   Lc : Pack.Lim_Comp;
   P  : Pack.Priv;

   task body Task1 is
   begin
      accept E do
         null;
      end E;
   end Task1;

   package body Pack is
      procedure Init (Lp : out Lim_Priv; Lc : in out Lim_Comp; P : out Priv) is
      begin
         Lp := 0;
         Lc := (others => 0);
         P  := (I => 0);
      end Init;
   end Pack;

begin
   Test
     ("C45291A",
      "CHECK THAT THE MEMBERSHIP TESTS YIELD CORRECT " &
      "RESULTS FOR TASK TYPES, LIMITED PRIVATE TYPES," &
      " COMPOSITE LIMITED TYPES, AND PRIVATE TYPES " &
      "WITHOUT DISCRIMINANTS");

   Pack.Init (Lp, Lc, P);

   if not Ident_Bool (T1 in Task1) then
      Failed ("INCORRECT VALUE OF 'T1 IN TASK1'");
   end if;

   if Ident_Bool (T1 not in Task1) then
      Failed ("INCORRECT VALUE OF 'T1 NOT IN TASK1'");
   end if;

   if not Ident_Bool (Lp in Pack.Lim_Priv) then
      Failed ("INCORRECT VALUE OF 'LP IN LIM_PRIV'");
   end if;

   if Ident_Bool (Lp not in Pack.Lim_Priv) then
      Failed ("INCORRECT VALUE OF 'LP NOT IN LIM_PRIV'");
   end if;

   if not Ident_Bool (Lc in Pack.Lim_Comp) then
      Failed ("INCORRECT VALUE OF 'LC IN LIM_COMP'");
   end if;

   if Ident_Bool (Lc not in Pack.Lim_Comp) then
      Failed ("INCORRECT VALUE OF 'LC NOT IN LIM_COMP'");
   end if;

   if not Ident_Bool (P in Pack.Priv) then
      Failed ("INCORRECT VALUE OF 'P IN PRIV'");
   end if;

   if Ident_Bool (P not in Pack.Priv) then
      Failed ("INCORRECT VALUE OF 'P NOT IN PRIV'");
   end if;

   if not Ident_Bool (T1 in Sub_Task1) then
      Failed ("INCORRECT VALUE OF 'T1 IN SUB_TASK1'");
   end if;

   if Ident_Bool (T1 not in Sub_Task1) then
      Failed ("INCORRECT VALUE OF 'T1 NOT IN SUB_TASK1'");
   end if;

   if not Ident_Bool (Lp in Sub_Lim_Priv) then
      Failed ("INCORRECT VALUE OF 'LP IN SUB_LIM_PRIV'");
   end if;

   if Ident_Bool (Lp not in Sub_Lim_Priv) then
      Failed ("INCORRECT VALUE OF 'LP NOT IN SUB_LIM_PRIV'");
   end if;

   if not Ident_Bool (Lc in Sub_Lim_Comp) then
      Failed ("INCORRECT VALUE OF 'LC IN SUB_LIM_COMP'");
   end if;

   if Ident_Bool (Lc not in Sub_Lim_Comp) then
      Failed ("INCORRECT VALUE OF 'LC NOT IN SUB_LIM_COMP'");
   end if;

   if not Ident_Bool (P in Sub_Priv) then
      Failed ("INCORRECT VALUE OF 'P IN SUB_PRIV'");
   end if;

   if Ident_Bool (P not in Sub_Priv) then
      Failed ("INCORRECT VALUE OF 'P NOT IN SUB_PRIV'");
   end if;

   T1.E;

   Result;

end C45291a;
