-- CC3231A.ADA

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
--      ACTUAL PARAMETER AN INTEGER TYPE, AND OPERATIONS OF THE FORMAL
--      TYPE ARE IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL
--      TYPE.

-- HISTORY:
--      TBN 09/14/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cc3231a is

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
     ("CC3231A",
      "CHECK THAT A PRIVATE OR LIMITED PRIVATE " &
      "FORMAL TYPE DENOTES ITS ACTUAL PARAMETER AN " &
      "INTEGER TYPE, AND OPERATIONS OF THE " &
      "FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING " &
      "OPERATIONS OF THE ACTUAL TYPE");

   declare  -- PRIVATE TYPE.
      type Fixed is delta 0.125 range 0.0 .. 10.0;

      Obj_Int : Integer := 1;
      Obj_Flo : Float   := 1.0;
      Obj_Fix : Fixed   := 1.0;

      package P1 is new P (Integer);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(1);
      if Pac_Var /= Obj_Int then
         Failed ("INCORRECT RESULTS - 1");
      end if;
      Obj_Int := Pac_Var + Obj_Int;
      if Obj_Int <= Pac_Var then
         Failed ("INCORRECT RESULTS - 2");
      end if;
      Pac_Var := Pac_Var * Obj_Int;
      if Pac_Var not in Integer then
         Failed ("INCORRECT RESULTS - 3");
      end if;
      if Obj_Int not in Sub_T then
         Failed ("INCORRECT RESULTS - 4");
      end if;
      if Integer'Pos (2) /= Sub_T'Pos (2) then
         Failed ("INCORRECT RESULTS - 5");
      end if;
      Pac_Var := 1;
      Obj_Fix := Pac_Var * Obj_Fix;
      if Obj_Fix /= 1.0 then
         Failed ("INCORRECT RESULTS - 6");
      end if;
      Obj_Int := 1;
      Obj_Fix := Obj_Fix / Obj_Int;
      if Obj_Fix /= 1.0 then
         Failed ("INCORRECT RESULTS - 7");
      end if;
      Obj_Int := Obj_Int**Pac_Var;
      if Obj_Int /= 1 then
         Failed ("INCORRECT RESULTS - 8");
      end if;
      Obj_Flo := Obj_Flo**Pac_Var;
      if Obj_Flo /= 1.0 then
         Failed ("INCORRECT RESULTS - 9");
      end if;
      Obj_Newt := 1;
      Obj_Newt := Obj_Newt - 1;
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 10");
      end if;
      if New_T'Succ (2) /= 3 then
         Failed ("INCORRECT RESULTS - 11");
      end if;
   end;

   declare  -- LIMITED PRIVATE TYPE.
      type Fixed is delta 0.125 range 0.0 .. 10.0;

      Obj_Int : Integer := 1;
      Obj_Flo : Float   := 1.0;
      Obj_Fix : Fixed   := 1.0;

      package P1 is new Lp (Integer);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(1);
      if Pac_Var /= Obj_Int then
         Failed ("INCORRECT RESULTS - 12");
      end if;
      Obj_Int := Pac_Var + Obj_Int;
      if Obj_Int <= Pac_Var then
         Failed ("INCORRECT RESULTS - 13");
      end if;
      Pac_Var := Pac_Var * Obj_Int;
      if Pac_Var not in Integer then
         Failed ("INCORRECT RESULTS - 14");
      end if;
      if Obj_Int not in Sub_T then
         Failed ("INCORRECT RESULTS - 15");
      end if;
      if Integer'Pos (2) /= Sub_T'Pos (2) then
         Failed ("INCORRECT RESULTS - 16");
      end if;
      Pac_Var := 1;
      Obj_Fix := Pac_Var * Obj_Fix;
      if Obj_Fix /= 1.0 then
         Failed ("INCORRECT RESULTS - 17");
      end if;
      Obj_Int := 1;
      Obj_Fix := Obj_Fix / Obj_Int;
      if Obj_Fix /= 1.0 then
         Failed ("INCORRECT RESULTS - 18");
      end if;
      Obj_Int := Obj_Int**Pac_Var;
      if Obj_Int /= 1 then
         Failed ("INCORRECT RESULTS - 19");
      end if;
      Obj_Flo := Obj_Flo**Pac_Var;
      if Obj_Flo /= 1.0 then
         Failed ("INCORRECT RESULTS - 20");
      end if;
      Obj_Newt := 1;
      Obj_Newt := Obj_Newt - 1;
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 21");
      end if;
      if New_T'Succ (2) /= 3 then
         Failed ("INCORRECT RESULTS - 22");
      end if;
   end;

   Result;
end Cc3231a;
