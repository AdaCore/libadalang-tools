-- C48006B.ADA

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
-- CHECK THAT AN ALLOCATOR OF THE FORM "NEW T'(X)" ALLOCATES A NEW
-- OBJECT EACH TIME IT IS EXECUTED AND THAT IF T IS A RECORD, ARRAY, OR
-- PRIVATE TYPE (CONSTRAINED OR UNCONSTRAINED), THE ALLOCATED OBJECT HAS
-- THE VALUE OF (X).

-- RM  01/14/80
-- RM  01/O1/82
-- SPS 10/27/82
-- EG  07/05/84
-- JBG 11/08/85 AVOID CONFLICT WITH AI-7 OR AI-275

with Report;

procedure C48006b is

   use Report;

begin

   Test
     ("C48006B",
      "CHECK THAT THE FORM 'NEW T'(X)' " &
      "ALLOCATES A NEW OBJECT " &
      "AND THAT IF T IS A RECORD, ARRAY, OR PRIVATE " &
      "TYPE, THE ALLOCATED OBJECT HAS THE VALUE (X)");

   -- RECORD OR ARRAY TYPE (CONSTRAINED OR UNCONSTRAINED)

   declare

      type Tb0 (A, B : Integer) is record
         C : Integer := 7;
      end record;
      subtype Tb is Tb0 (2, 3);
      type Atb is access Tb;
      type Atb0 is access Tb0;
      Vb1, Vb2   : Atb;
      Vb01, Vb02 : Atb0;

      type Arr0 is array (Integer range <>) of Integer;
      subtype Arr is Arr0 (1 .. 4);
      type A_Arr is access Arr;
      type A_Arr0 is access Arr0;
      Varr1, Varr2   : A_Arr;
      Varr01, Varr02 : A_Arr0;

   begin

      Vb1 := new Tb'(2, 3, 5);
      if
        (Vb1.A /= Ident_Int (2) or
         Vb1.B /= Ident_Int (3) or
         Vb1.C /= Ident_Int (5))
      then
         Failed ("WRONG VALUES  -  B1 1");
      end if;

      Vb2 := new Tb'(Ident_Int (2), Ident_Int (3), Ident_Int (6));
      if
        (Vb2.A /= 2 or
         Vb2.B /= 3 or
         Vb2.C /= 6 or
         Vb1.A /= 2 or
         Vb1.B /= 3 or
         Vb1.C /= 5)
      then
         Failed ("WRONG VALUES  -  B1 2");
      end if;

      Vb01 := new Tb0'(1, 2, 3);
      if
        (Vb01.A /= Ident_Int (1) or
         Vb01.B /= Ident_Int (2) or
         Vb01.C /= Ident_Int (3))
      then
         Failed ("WRONG VALUES  -  B2 1");
      end if;

      Vb02 := new Tb0'(Ident_Int (4), Ident_Int (5), Ident_Int (6));
      if
        (Vb02.A /= Ident_Int (4) or
         Vb02.B /= Ident_Int (5) or
         Vb02.C /= Ident_Int (6) or
         Vb01.A /= Ident_Int (1) or
         Vb01.B /= Ident_Int (2) or
         Vb01.C /= Ident_Int (3))
      then
         Failed ("WRONG VALUES  -  B2 2");
      end if;

      Varr1 := new Arr'(5, 6, 7, 8);
      if
        (Varr1 (1) /= Ident_Int (5) or
         Varr1 (2) /= Ident_Int (6) or
         Varr1 (3) /= Ident_Int (7) or
         Varr1 (4) /= Ident_Int (8))
      then
         Failed ("WRONG VALUES  -  B3 1");
      end if;

      Varr2 :=
        new Arr'(Ident_Int (1), Ident_Int (2), Ident_Int (3), Ident_Int (4));
      if
        (Varr2 (1) /= 1 or
         Varr2 (2) /= 2 or
         Varr2 (3) /= 3 or
         Varr2 (4) /= 4 or
         Varr1 (1) /= 5 or
         Varr1 (2) /= 6 or
         Varr1 (3) /= 7 or
         Varr1 (4) /= 8)
      then
         Failed ("WRONG VALUES  -  B3 2");
      end if;

      Varr01 := new Arr0'(11, 12, 13);
      if
        (Varr01 (Integer'First) /= Ident_Int (11) or
         Varr01 (Integer'First + 1) /= Ident_Int (12) or
         Varr01 (Integer'First + 2) /= Ident_Int (13))
      then
         Failed ("WRONG VALUES -  B4 1");
      end if;
      if
        (Varr01.all'First /= Ident_Int (Integer'First) or
         Varr01.all'Last /= Ident_Int (Integer'First + 2))
      then
         Failed ("WRONG VALUES -  B4 2");
      end if;

      Varr02 := new Arr0'(1 => Ident_Int (14), 2 => Ident_Int (15));
      if
        (Varr02 (1) /= 14 or
         Varr02 (2) /= 15 or
         Varr01 (Integer'First) /= 11 or
         Varr01 (Integer'First + 1) /= 12 or
         Varr01 (Integer'First + 2) /= 13)
      then
         Failed ("WRONG VALUES -  B4 3");
      end if;

   end;

   -- PRIVATE TYPE (CONSTRAINED OR UNCONSTRAINED)

   declare

      package P is
         type Up (A, B : Integer) is private;
--             SUBTYPE CP IS UP(1, 2);
--             TYPE A_CP IS ACCESS CP;
         type A_Up is access Up;
         Cons1_Up : constant Up;
         Cons2_Up : constant Up;
         Cons3_Up : constant Up;
         Cons4_Up : constant Up;
--             PROCEDURE CHECK1 (X : A_CP);
--             PROCEDURE CHECK2 (X, Y : A_CP);
         procedure Check3 (X : A_Up);
         procedure Check4 (X, Y : A_Up);
      private
         type Up (A, B : Integer) is record
            C : Integer;
         end record;
         Cons1_Up : constant Up := (1, 2, 3);
         Cons2_Up : constant Up :=
           (Ident_Int (1), Ident_Int (2), Ident_Int (4));
         Cons3_Up : constant Up := (7, 8, 9);
         Cons4_Up : constant Up :=
           (Ident_Int (10), Ident_Int (11), Ident_Int (12));
      end P;

      use P;

--        V_A_CP1, V_A_CP2 : A_CP;
      V_A_Up1, V_A_Up2 : A_Up;

      package body P is
--             PROCEDURE CHECK1 (X : A_CP) IS
--             BEGIN
--                  IF (X.A /= IDENT_INT(1) OR
--                      X.B /= IDENT_INT(2) OR
--                      X.C /= IDENT_INT(3)) THEN
--                       FAILED ("WRONG VALUES - CP1");
--                  END IF;
--             END CHECK1;
--             PROCEDURE CHECK2 (X, Y : A_CP) IS
--             BEGIN
--                  IF (X.A /= 1 OR X.B /= 2 OR X.C /= 3 OR
--                      Y.A /= 1 OR Y.B /= 2 OR Y.C /= 4) THEN
--                       FAILED ("WRONG VALUES - CP2");
--                  END IF;
--             END CHECK2;
         procedure Check3 (X : A_Up) is
         begin
            if
              (X.A /= Ident_Int (7) or
               X.B /= Ident_Int (8) or
               X.C /= Ident_Int (9))
            then
               Failed ("WRONG VALUES - UP1");
            end if;
         end Check3;
         procedure Check4 (X, Y : A_Up) is
         begin
            if
              (X.A /= 7 or
               X.B /= 8 or
               X.C /= 9 or
               Y.A /= 10 or
               Y.B /= 11 or
               Y.C /= 12)
            then
               Failed ("WRONG VALUES - UP2");
            end if;
         end Check4;
      end P;

   begin

--        V_A_CP1 := NEW CP'(CONS1_UP);
--        CHECK1(V_A_CP1);

--        V_A_CP2 := NEW CP'(CONS2_UP);
--        CHECK2(V_A_CP1, V_A_CP2);

      V_A_Up1 := new P.Up'(Cons3_Up);
      Check3 (V_A_Up1);

      V_A_Up2 := new P.Up'(Cons4_Up);
      Check4 (V_A_Up1, V_A_Up2);

   end;

   Result;

end C48006b;
