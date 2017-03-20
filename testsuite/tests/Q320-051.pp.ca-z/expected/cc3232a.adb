-- CC3232A.ADA

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
--     ACTUAL PARAMETER A FLOATING POINT TYPE, AND OPERATIONS OF THE
--     FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE
--     ACTUAL TYPE.

-- HISTORY:
--     TBN 09/15/88  CREATED ORIGINAL TEST.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
procedure Cc3232a is

   type Float is digits 5 range 0.0 .. 10.0;

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

   function Ident_Flo (X : Float) return Float is
   begin
      if Equal (3, 3) then
         return X;
      else
         return (0.0);
      end if;
   end Ident_Flo;

begin
   Test
     ("CC3232A",
      "CHECK THAT A PRIVATE OR LIMITED PRIVATE " &
      "FORMAL TYPE DENOTES ITS ACTUAL PARAMETER A " &
      "FLOATING POINT TYPE, AND OPERATIONS OF THE " &
      "FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING " &
      "OPERATIONS OF THE ACTUAL TYPE");

   declare  -- PRIVATE TYPE.
      Obj_Int : Integer := 1;
      Obj_Flo : Float   := 1.0;

      package P1 is new P (Float);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(1.0);
      if Pac_Var /= Obj_Flo then
         Failed ("INCORRECT RESULTS - 1");
      end if;
      Obj_Flo := Ident_Flo (Pac_Var) + Ident_Flo (Obj_Flo);
      if Obj_Flo <= Pac_Var then
         Failed ("INCORRECT RESULTS - 2");
      end if;
      Pac_Var := Pac_Var * Obj_Flo;
      if Pac_Var not in Float then
         Failed ("INCORRECT RESULTS - 3");
      end if;
      if Obj_Flo not in Sub_T then
         Failed ("INCORRECT RESULTS - 4");
      end if;
      Pac_Var := 1.0;
      Obj_Flo := 1.0;
      Obj_Flo := Pac_Var * Obj_Flo;
      if Obj_Flo /= 1.0 then
         Failed ("INCORRECT RESULTS - 5");
      end if;
      Obj_Flo := 1.0;
      Obj_Flo := Obj_Flo / Obj_Flo;
      if Obj_Flo /= 1.0 then
         Failed ("INCORRECT RESULTS - 6");
      end if;
      Pac_Var := 1.0;
      Obj_Flo := Pac_Var**Obj_Int;
      if Obj_Flo /= 1.0 then
         Failed ("INCORRECT RESULTS - 7");
      end if;
      if Sub_T'Digits /= 5 then
         Failed ("INCORRECT RESULTS - 8");
      end if;
      Obj_Newt := 1.0;
      Obj_Newt := Obj_Newt - 1.0;
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 9");
      end if;
      if New_T'Digits /= 5 then
         Failed ("INCORRECT RESULTS - 10");
      end if;
   end;

   declare  -- LIMITED PRIVATE TYPE.
      Obj_Int : Integer := 1;
      Obj_Flo : Float   := 1.0;

      package P1 is new Lp (Float);
      use P1;

      type New_T is new Sub_T;
      Obj_Newt : New_T;
   begin
      Pac_Var := Sub_T'(1.0);
      if Pac_Var /= Obj_Flo then
         Failed ("INCORRECT RESULTS - 1");
      end if;
      Obj_Flo := Ident_Flo (Pac_Var) + Ident_Flo (Obj_Flo);
      if Obj_Flo <= Pac_Var then
         Failed ("INCORRECT RESULTS - 2");
      end if;
      Pac_Var := Pac_Var * Obj_Flo;
      if Pac_Var not in Float then
         Failed ("INCORRECT RESULTS - 3");
      end if;
      if Obj_Flo not in Sub_T then
         Failed ("INCORRECT RESULTS - 4");
      end if;
      Pac_Var := 1.0;
      Obj_Flo := 1.0;
      Obj_Flo := Pac_Var * Obj_Flo;
      if Obj_Flo /= 1.0 then
         Failed ("INCORRECT RESULTS - 5");
      end if;
      Obj_Flo := 1.0;
      Obj_Flo := Obj_Flo / Obj_Flo;
      if Obj_Flo /= 1.0 then
         Failed ("INCORRECT RESULTS - 6");
      end if;
      Pac_Var := 1.0;
      Obj_Flo := Pac_Var**Obj_Int;
      if Obj_Flo /= 1.0 then
         Failed ("INCORRECT RESULTS - 7");
      end if;
      if Sub_T'Digits /= 5 then
         Failed ("INCORRECT RESULTS - 8");
      end if;
      Obj_Newt := 1.0;
      Obj_Newt := Obj_Newt - 1.0;
      if Obj_Newt not in New_T then
         Failed ("INCORRECT RESULTS - 9");
      end if;
      if New_T'Digits /= 5 then
         Failed ("INCORRECT RESULTS - 10");
      end if;
   end;

   Result;
end Cc3232a;
