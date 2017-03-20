-- CC3234A.ADA

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
--      ACTUAL PARAMETER AN ARRAY TYPE, AND OPERATIONS OF THE FORMAL
--      TYPE ARE IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL
--      TYPE.

-- HISTORY:
--      TBN 09/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cc3234a is

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
     ("CC3234A",
      "CHECK THAT A PRIVATE OR LIMITED PRIVATE " &
      "FORMAL TYPE DENOTES ITS ACTUAL PARAMETER AN " &
      "ARRAY TYPE, AND OPERATIONS OF THE " &
      "FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING " &
      "OPERATIONS OF THE ACTUAL TYPE");

   declare  -- PRIVATE TYPE.
      type Array_Type is array (1 .. 10) of Integer;

      Obj_Arr : Array_Type := (others => 1);

      package P1 is new P (Array_Type);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
      if Pac_Var /= Obj_Arr then
         Failed ("INCORRECT RESULTS - 1");
      end if;
      Obj_Arr (1) := Pac_Var (2) + Obj_Arr (1);
      if Obj_Arr (1) <= Pac_Var (1) then
         Failed ("INCORRECT RESULTS - 2");
      end if;
      Pac_Var (1) := Pac_Var (1) * Obj_Arr (3);
      if Pac_Var not in Array_Type then
         Failed ("INCORRECT RESULTS - 3");
      end if;
      if Obj_Arr not in Sub_T then
         Failed ("INCORRECT RESULTS - 4");
      end if;
      if Array_Type'First /= Sub_T'First then
         Failed ("INCORRECT RESULTS - 5");
      end if;
      Obj_Arr (1 .. 5) := Pac_Var (6 .. 10);
      if Obj_Arr (1 .. 5) /= (1, 1, 1, 1, 1) then
         Failed ("INCORRECT RESULTS - 6");
      end if;
      Pac_Var  := (1, 1, 1, 1, 1, 2, 2, 2, 2, 2);
      Obj_Newt := (1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
      Obj_Newt := New_T (Pac_Var);
      if Obj_Newt (3 .. 7) /= (1, 1, 1, 2, 2) then
         Failed ("INCORRECT RESULTS - 7");
      end if;
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 8");
      end if;
   end;

   declare  -- LIMITED PRIVATE TYPE.
      type Array_Type is array (1 .. 10) of Integer;

      Obj_Arr : Array_Type := (others => 1);

      package P1 is new Lp (Array_Type);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
      if Pac_Var /= Obj_Arr then
         Failed ("INCORRECT RESULTS - 9");
      end if;
      Obj_Arr (1) := Pac_Var (2) + Obj_Arr (1);
      if Obj_Arr (1) <= Pac_Var (1) then
         Failed ("INCORRECT RESULTS - 10");
      end if;
      Pac_Var (1) := Pac_Var (1) * Obj_Arr (3);
      if Pac_Var not in Array_Type then
         Failed ("INCORRECT RESULTS - 11");
      end if;
      if Obj_Arr not in Sub_T then
         Failed ("INCORRECT RESULTS - 12");
      end if;
      if Array_Type'First /= Sub_T'First then
         Failed ("INCORRECT RESULTS - 13");
      end if;
      Obj_Arr (1 .. 5) := Pac_Var (6 .. 10);
      if Obj_Arr (1 .. 5) /= (1, 1, 1, 1, 1) then
         Failed ("INCORRECT RESULTS - 14");
      end if;
      Pac_Var  := (1, 1, 1, 1, 1, 2, 2, 2, 2, 2);
      Obj_Newt := (1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
      Obj_Newt := New_T (Pac_Var);
      if Obj_Newt (3 .. 7) /= (1, 1, 1, 2, 2) then
         Failed ("INCORRECT RESULTS - 15");
      end if;
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 16");
      end if;
   end;

   Result;
end Cc3234a;
