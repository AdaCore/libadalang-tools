-- CC3233A.ADA

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
--     CHECK THAT A PRIVATE OR LIMITED PRIVATE FORMAL TYPE DENOTES ITS
--     ACTUAL PARAMETER, A FIXED POINT TYPE AND OPERATIONS OF THE FORMAL
--     TYPE ARE IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL
--     TYPE.

-- HISTORY:
--     TBN 09/15/88  CREATED ORIGINAL TEST.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
procedure Cc3233a is

   type Fixed is delta 0.125 range 0.0 .. 10.0;

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

   function Ident_Fix (X : Fixed) return Fixed is
   begin
      if Equal (3, 3) then
         return X;
      else
         return (0.0);
      end if;
   end Ident_Fix;

begin
   Test
     ("CC3233A",
      "CHECK THAT A PRIVATE OR LIMITED PRIVATE " &
      "FORMAL TYPE DENOTES ITS ACTUAL PARAMETER, A " &
      "FIXED POINT TYPE AND OPERATIONS OF THE FORMAL " &
      "TYPE ARE IDENTIFIED WITH CORRESPONDING " &
      "OPERATIONS OF THE ACTUAL TYPE");

   declare  -- PRIVATE TYPE.
      Obj_Int : Integer := 1;
      Obj_Fix : Fixed   := 1.0;

      package P1 is new P (Fixed);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(1.0);
      if Pac_Var /= Obj_Fix then
         Failed ("INCORRECT RESULTS - 1");
      end if;
      Obj_Fix := Ident_Fix (Pac_Var) + Ident_Fix (Obj_Fix);
      if Obj_Fix <= Pac_Var then
         Failed ("INCORRECT RESULTS - 2");
      end if;
      Pac_Var := Obj_Int * Obj_Fix;
      if Pac_Var not in Fixed then
         Failed ("INCORRECT RESULTS - 3");
      end if;
      if Obj_Fix not in Sub_T then
         Failed ("INCORRECT RESULTS - 4");
      end if;
      if Sub_T'Delta /= 0.125 then
         Failed ("INCORRECT RESULTS - 5");
      end if;
      Obj_Newt := 1.0;
      Obj_Newt := Obj_Newt - 1.0;
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 6");
      end if;
      if New_T'Delta /= 0.125 then
         Failed ("INCORRECT RESULTS - 7");
      end if;
      Obj_Newt := New_T'Small + 1.0;
      Obj_Fix  := 1.0;
      Obj_Fix  := Fixed (Obj_Fix * Obj_Fix);
      if Obj_Fix /= 1.0 then
         Failed ("INCORRECT RESULTS - 8");
      end if;
      Obj_Fix := 1.0;
      Obj_Fix := Sub_T (Obj_Fix / Obj_Fix);
      if Obj_Fix /= 1.0 then
         Failed ("INCORRECT RESULTS - 9");
      end if;
      if Fixed'Small /= New_T'Small then
         Failed ("INCORRECT RESULTS - 10");
      end if;
   end;

   declare  -- LIMITED PRIVATE TYPE.
      Obj_Int : Integer := 1;
      Obj_Fix : Fixed   := 1.0;

      package P1 is new Lp (Fixed);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(1.0);
      if Pac_Var /= Obj_Fix then
         Failed ("INCORRECT RESULTS - 1");
      end if;
      Obj_Fix := Ident_Fix (Pac_Var) + Ident_Fix (Obj_Fix);
      if Obj_Fix <= Pac_Var then
         Failed ("INCORRECT RESULTS - 2");
      end if;
      Pac_Var := Obj_Int * Obj_Fix;
      if Pac_Var not in Fixed then
         Failed ("INCORRECT RESULTS - 3");
      end if;
      if Obj_Fix not in Sub_T then
         Failed ("INCORRECT RESULTS - 4");
      end if;
      if Sub_T'Delta /= 0.125 then
         Failed ("INCORRECT RESULTS - 5");
      end if;
      Obj_Newt := 1.0;
      Obj_Newt := Obj_Newt - 1.0;
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 6");
      end if;
      if New_T'Delta /= 0.125 then
         Failed ("INCORRECT RESULTS - 7");
      end if;
      Obj_Newt := New_T'Small + 1.0;
      Obj_Fix  := 1.0;
      Obj_Fix  := Fixed (Obj_Fix * Obj_Fix);
      if Obj_Fix /= 1.0 then
         Failed ("INCORRECT RESULTS - 8");
      end if;
      Obj_Fix := 1.0;
      Obj_Fix := Sub_T (Obj_Fix / Obj_Fix);
      if Obj_Fix /= 1.0 then
         Failed ("INCORRECT RESULTS - 9");
      end if;
      if Fixed'Small /= New_T'Small then
         Failed ("INCORRECT RESULTS - 10");
      end if;
   end;

   Result;
end Cc3233a;
