-- C74407B.ADA

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
--     CHECK, FOR A LIMITED PRIVATE TYPE, THAT PRE-DEFINED EQUALITY AND
--     ASSIGNMENT ARE DEFINED AND AVAILABLE WITHIN THE PRIVATE PART AND
--     THE BODY OF A PACKAGE, AFTER THE FULL DECLARATION, IF THE FULL
--     DECLARATION IS NOT LIMITED.

-- HISTORY:
--     BCB 07/15/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C74407b is

   package Pp is
      type Priv is private;
      C1 : constant Priv;
      C2 : constant Priv;
   private
      type Priv is (One, Two, Three, Four, Five, Six);
      C1 : constant Priv := One;
      C2 : constant Priv := Two;
   end Pp;

   use Pp;

   package P is
      type Int is limited private;
      type Comp is limited private;
      type Der is limited private;
   private
      type Int is range 1 .. 100;
      type Comp is array (1 .. 5) of Integer;
      type Der is new Priv;
      D, E       : Int              := 10;
      F          : Int              := 20;
      Cons_Int1  : constant Int     := 30;
      G          : Boolean          := D = E;
      H          : Boolean          := D /= F;
      Cons_Bool1 : constant Boolean := D = E;
      Cons_Bool2 : constant Boolean := D /= F;
      I          : Comp             := (1, 2, 3, 4, 5);
      Cons_Comp1 : constant Comp    := (6, 7, 8, 9, 10);
      J          : Der              := Der (C1);
      Cons_Der1  : constant Der     := Der (C2);
   end P;

   package body P is
      A, B, C    : Int;
      X, Y, Z    : Comp;
      L, M, N    : Der;
      Cons_Int2  : constant Int  := 10;
      Cons_Comp2 : constant Comp := (1, 2, 3, 4, 5);
      Cons_Der2  : constant Der  := Der (C1);
   begin
      Test
        ("C74407B",
         "CHECK, FOR A LIMITED PRIVATE TYPE, THAT " &
         "PRE-DEFINED EQUALITY AND ASSIGNMENT ARE " &
         "DEFINED AND AVAILABLE WITHIN THE PRIVATE " &
         "PART AND THE BODY OF A PACKAGE, AFTER " &
         "THE FULL DECLARATION, IF THE FULL " & "DECLARATION IS NOT LIMITED");

      A := 10;

      B := 10;

      C := 20;

      if A = C then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 1");
      end if;

      if A /= B then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 1");
      end if;

      if Cons_Int2 = C then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 2");
      end if;

      if Cons_Int2 /= B then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 2");
      end if;

      if not G then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
            "OPERATION WITHIN THE PRIVATE PART OF THE " & "PACKAGE - 1");
      end if;

      if not H then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
            "OPERATION WITHIN THE PRIVATE PART OF THE " & "PACKAGE - 1");
      end if;

      if not Cons_Bool1 then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
            "OPERATION WITHIN THE PRIVATE PART OF THE " & "PACKAGE - 2");
      end if;

      if not Cons_Bool2 then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
            "OPERATION WITHIN THE PRIVATE PART OF THE " & "PACKAGE - 2");
      end if;

      X := (1, 2, 3, 4, 5);

      Y := (1, 2, 3, 4, 5);

      Z := (5, 4, 3, 2, 1);

      if X = Z then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 3");
      end if;

      if X /= Y then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 3");
      end if;

      if Cons_Comp2 = Z then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 4");
      end if;

      if Cons_Comp2 /= Y then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 4");
      end if;

      L := Der (C1);

      M := Der (C1);

      N := Der (C2);

      if L = N then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 5");
      end if;

      if L /= M then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 5");
      end if;

      if Cons_Der2 = N then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 6");
      end if;

      if Cons_Der2 /= M then
         Failed
           ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
            "OPERATION WITHIN THE PACKAGE BODY - 6");
      end if;

      Result;
   end P;

   use P;

begin
   null;
end C74407b;
