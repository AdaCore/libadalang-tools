-- CC3236A.ADA

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
--      CHECK THAT A FORMAL PRIVATE AND LIMITED PRIVATE TYPE DENOTES ITS
--      ACTUAL PARAMETER, AND OPERATIONS OF THE FORMAL TYPE ARE
--      IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL TYPE
--      WHEN THE ACTUAL PARAMETER IS A TYPE WITH DISCRIMINANTS.

-- HISTORY:
--      DHH 10/24/88 CREATED ORIGINAL TEST.
--      PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
procedure Cc3236a is

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
     ("CC3236A",
      "CHECK THAT A FORMAL PRIVATE OR LIMITED " &
      "PRIVATE TYPE DENOTES ITS ACTUAL PARAMETER AND " &
      "OPERATIONS OF THE FORMAL TYPE ARE IDENTIFIED " &
      "WITH CORRESPONDING OPERATIONS OF THE ACTUAL " &
      "TYPE, WHEN THE ACTUAL PARAMETER IS A TYPE " &
      "WITH DISCRIMINANTS");

   declare
      type Rec (X : Integer := 5) is record
         null;
      end record;
      Obj_Rec : Rec (4);

      package P2 is new P (Rec);
      use P2;

      type New_T is new Sub_T;
      Obj_Newt : New_T (4);
   begin
      Pac_Var := Sub_T'((X => 4));
      if Pac_Var /= Obj_Rec then
         Failed ("INCORRECT RESULTS - 1");
      end if;
      if Pac_Var not in Rec then
         Failed ("INCORRECT RESULTS - 2");
      end if;
      if Obj_Rec not in Sub_T then
         Failed ("INCORRECT RESULTS - 3");
      end if;
      if Pac_Var.X /= Obj_Newt.X then
         Failed ("INCORRECT RESULTS - 4");
      end if;
   end;

   declare
      type Rec (X : Integer := 5) is record
         null;
      end record;
      Obj_Rec : Rec (4);

      package P2 is new Lp (Rec);
      use P2;

      type New_T is new Sub_T;
      Obj_Newt : New_T (4);
   begin
      Pac_Var := Sub_T'(X => 4);
      if Pac_Var /= Obj_Rec then
         Failed ("INCORRECT RESULTS - 7");
      end if;
      if Pac_Var not in Rec then
         Failed ("INCORRECT RESULTS - 8");
      end if;
      if Obj_Rec not in Sub_T then
         Failed ("INCORRECT RESULTS - 9");
      end if;
      if Pac_Var.X /= Obj_Newt.X then
         Failed ("INCORRECT RESULTS - 10");
      end if;
   end;

   Result;
end Cc3236a;
