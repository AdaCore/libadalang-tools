-- CC3230A.ADA

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
--      ACTUAL PARAMETER AN ENUMERATION TYPE, AND OPERATIONS OF THE
--      FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE
--      ACTUAL TYPE.

-- HISTORY:
--      TBN 09/14/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cc3230a is

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
     ("CC3230A",
      "CHECK THAT A PRIVATE OR LIMITED PRIVATE " &
      "FORMAL TYPE DENOTES ITS ACTUAL PARAMETER AN " &
      "ENUMERATION TYPE, AND OPERATIONS OF THE " &
      "FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING " &
      "OPERATIONS OF THE ACTUAL TYPE");

   declare
      type Enum is (Red, Yellow, Green, Blue);
      Obj_Enu : Enum := Red;

      package P2 is new P (Enum);
      use P2;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(Red);
      if (Pac_Var < Obj_Enu) or (Pac_Var > Obj_Enu) then
         Failed ("INCORRECT RESULTS - 1");
      end if;
      if Pac_Var not in Enum then
         Failed ("INCORRECT RESULTS - 2");
      end if;
      if Obj_Enu not in Sub_T then
         Failed ("INCORRECT RESULTS - 3");
      end if;
      if Enum'Val (0) /= Sub_T'Val (0) then
         Failed ("INCORRECT RESULTS - 4");
      end if;
      Obj_Enu := Sub_T'Succ (Pac_Var);
      if Sub_T'Pos (Red) /= 0 and then Obj_Enu /= Blue then
         Failed ("INCORRECT RESULTS - 5");
      end if;
      Obj_Newt := Blue;
      Obj_Newt := New_T'Pred (Obj_Newt);
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 6");
      end if;
      if New_T'Width /= 6 then
         Failed ("INCORRECT RESULTS - 7");
      end if;
   end;

   declare
      type Enum is (Red, Yellow, Green, Blue);
      Obj_Enu : Enum := Red;

      package P2 is new Lp (Enum);
      use P2;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(Red);
      if (Pac_Var < Obj_Enu) or (Pac_Var > Obj_Enu) then
         Failed ("INCORRECT RESULTS - 8");
      end if;
      if Pac_Var not in Enum then
         Failed ("INCORRECT RESULTS - 9");
      end if;
      if Obj_Enu not in Sub_T then
         Failed ("INCORRECT RESULTS - 10");
      end if;
      if Enum'Val (0) /= Sub_T'Val (0) then
         Failed ("INCORRECT RESULTS - 11");
      end if;
      Obj_Enu := Sub_T'Succ (Pac_Var);
      if Sub_T'Pos (Red) /= 0 and then Obj_Enu /= Blue then
         Failed ("INCORRECT RESULTS - 12");
      end if;
      Obj_Newt := Blue;
      Obj_Newt := New_T'Pred (Obj_Newt);
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 13");
      end if;
      if New_T'Width /= 6 then
         Failed ("INCORRECT RESULTS - 14");
      end if;
   end;

   Result;
end Cc3230a;
