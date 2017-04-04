-- CC3235A.ADA

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
--      CHECK THAT A PRIVATE OR LIMITED PRIVATE FORMAL TYPE DENOTES ITS
--      ACTUAL PARAMETER AN ACCESS TYPE, AND OPERATIONS OF THE FORMAL
--      TYPE ARE IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL
--      TYPE.

-- HISTORY:
--      TBN 09/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cc3235a is

   generic
      type T is private;
   package P is
      subtype Sub_T is T;
      Pac_Var : T;
   end P;

   generic
      type T is limited private;
   package Lp is
      subtype Sub_T is T;
      Pac_Var : T;
   end Lp;

begin
   Test
     ("CC3235A",
      "CHECK THAT A PRIVATE OR LIMITED PRIVATE " &
      "FORMAL TYPE DENOTES ITS ACTUAL PARAMETER AN " &
      "ACCESS TYPE, AND OPERATIONS OF THE " &
      "FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING " &
      "OPERATIONS OF THE ACTUAL TYPE");

   declare  -- PRIVATE TYPE.
      type Enum is (Red, Yellow, Green, Blue);

      type Access_Type is access Enum;

      Obj_Acc : Access_Type := new Enum'(Red);

      package P1 is new P (Access_Type);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := new Enum'(Red);
      if (Pac_Var.all < Obj_Acc.all) or (Pac_Var.all > Obj_Acc.all) then
         Failed ("INCORRECT RESULTS - 1");
      end if;
      if Pac_Var not in Access_Type then
         Failed ("INCORRECT RESULTS - 2");
      end if;
      if Obj_Acc not in Sub_T then
         Failed ("INCORRECT RESULTS - 3");
      end if;
      Obj_Acc := new Enum'(Enum'Succ (Pac_Var.all));
      if Obj_Acc.all /= Yellow then
         Failed ("INCORRECT RESULTS - 4");
      end if;
      Obj_Newt := new Enum'(Blue);
      Obj_Newt := new Enum'(Enum'Pred (Obj_Newt.all));
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 5");
      end if;
   end;

   declare  -- LIMITED PRIVATE TYPE.
      type Enum is (Red, Yellow, Green, Blue);

      type Access_Type is access Enum;

      Obj_Acc : Access_Type := new Enum'(Red);

      package P1 is new Lp (Access_Type);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := new Enum'(Red);
      if (Pac_Var.all < Obj_Acc.all) or (Pac_Var.all > Obj_Acc.all) then
         Failed ("INCORRECT RESULTS - 6");
      end if;
      if Pac_Var not in Access_Type then
         Failed ("INCORRECT RESULTS - 7");
      end if;
      if Obj_Acc not in Sub_T then
         Failed ("INCORRECT RESULTS - 8");
      end if;
      Obj_Acc := new Enum'(Enum'Succ (Pac_Var.all));
      if Obj_Acc.all /= Yellow then
         Failed ("INCORRECT RESULTS - 9");
      end if;
      Obj_Newt := new Enum'(Blue);
      Obj_Newt := new Enum'(Enum'Pred (Obj_Newt.all));
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 10");
      end if;
   end;

   Result;
end Cc3235a;
