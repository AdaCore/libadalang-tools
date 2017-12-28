-- C43210A.ADA

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
-- CHECK THAT A NON-AGGREGATE EXPRESSION IN A NAMED COMPONENT ASSOCIATION IS
-- EVALUATED ONCE FOR EACH COMPONENT SPECIFIED BY THE ASSOCIATION.

-- EG  02/02/84

with Report;

procedure C43210a is

   use Report;

begin

   Test
     ("C43210A",
      "CHECK THAT A NON-AGGREGATE IN A NAMED " &
      "COMPONENT ASSOCIATION IS EVALUATED ONCE " &
      "FOR EACH COMPONENT SPECIFIED BY THE " &
      "ASSOCIATION");

   declare

      type T1 is array (1 .. 10) of Integer;
      type T2 is array (1 .. 8, 1 .. 2) of Integer;
      type T3 is array (1 .. 2, 1 .. 8) of Integer;
      type T4 is array (1 .. 8, 1 .. 8) of Integer;

      A1 : T1;
      A2 : T2;
      A3 : T3;
      A4 : T4;
      Cc : Integer;

      function Calc (A : Integer) return Integer is
      begin
         Cc := Cc + 1;
         return Ident_Int (A);
      end Calc;

      procedure Check (A : String; B : Integer) is
      begin
         if Cc /= B then
            Failed
              ("CASE " &
               A &
               " : INCORRECT NUMBER OF " &
               "EVALUATIONS. NUMBER OF EVALUATIONS " &
               "SHOULD BE " &
               Integer'Image (B) &
               ", BUT IS " &
               Integer'Image (Cc));
         end if;
      end Check;

   begin

      Case_A :
      begin

         Cc := 0;
         A1 := T1'(4 .. 5 => Calc (2), 6 .. 8 => Calc (4), others => 5);
         Check ("A", 5);

      end Case_A;

      Case_B :
      begin

         Cc := 0;
         A1 := T1'(1 | 4 .. 6 | 3 | 2 => Calc (-1), others => -2);
         Check ("B", 6);

      end Case_B;

      Case_C :
      begin

         Cc := 0;
         A1 := T1'(1 | 3 | 5 | 7 .. 9 => -1, others => Calc (-2));
         Check ("C", 4);

      end Case_C;

      Case_D :
      begin

         Cc := 0;
         A2 :=
           T2'
             (4 .. 6 | 8 | 2 .. 3 => (1 .. 2 => Calc (1)),
              others => (1 .. 2 => -1));
         Check ("D", 12);

      end Case_D;

      Case_E :
      begin

         Cc := 0;
         A3 := T3'(1 .. 2 => (2 | 4 | 6 .. 8 => Calc (-1), others => -2));
         Check ("E", 10);

      end Case_E;

      Case_F :
      begin

         Cc := 0;
         A4 :=
           T4'
             (7 .. 8 | 3 .. 5 =>
                (1 | 2 | 4 | 6 .. 8 => Calc (1), others => -2),
              others => (others => -2));
         Check ("F", 30);

      end Case_F;

      Case_G :
      begin

         Cc := 0;
         A4 :=
           T4'
             (5 .. 8 | 3 | 1 => (7 | 1 .. 5 | 8 => -1, others => Calc (-2)),
              others => (others => Calc (-2)));
         Check ("G", 22);

      end Case_G;

   end;

   Result;

end C43210a;
