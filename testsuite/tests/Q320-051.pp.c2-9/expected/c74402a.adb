-- C74402A.ADA

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
-- CHECK THAT A SUBPROGRAM PARAMETER OF A LIMITED TYPE MAY HAVE A DEFAULT
-- EXPRESSION, EVEN IF THE SUBPROGRAM IS DECLARED OUTSIDE THE PACKAGE THAT
-- DECLARES THE LIMITED TYPE. (SEE ALSO 6.4.2/T1 FOR TESTS OF OTHER LIMITED
-- TYPES.)

-- DSJ 5/6/83
-- SPS 10/24/83

with Report;
procedure C74402a is

   use Report;

begin

   Test
     ("C74402A",
      "CHECK THAT A SUBPROGRAM PARAMETER OF A LIMITED " &
      "TYPE MAY HAVE A DEFAULT EXPRESSION, EVEN IF " &
      "THE SUBPROGRAM IS DECLARED OUTSIDE THE PACKAGE " &
      "THAT DECLARES THE LIMITED TYPE");

   declare

      package Pack1 is

         type Lp1 is limited private;
         type Lp2 is array (1 .. 2) of Lp1;
         type Lp3 is record
            C1, C2 : Lp2;
         end record;

         function F1 return Lp1;
         function F2 return Lp2;
         function F3 return Lp3;

         procedure G1 (X : Lp1 := F1);      -- LEGAL
         procedure G2 (X : Lp2 := F2);      -- LEGAL
         procedure G3 (X : Lp3 := F3);      -- LEGAL

      private

         type Lp1 is new Integer;

      end Pack1;

      package body Pack1 is

         function F1 return Lp1 is
         begin
            return Lp1'(1);
         end F1;

         function F2 return Lp2 is
         begin
            return Lp2'(2, 3);
         end F2;

         function F3 return Lp3 is
         begin
            return Lp3'((4, 5), (6, 7));
         end F3;

         procedure G1 (X : Lp1 := F1) is
         begin
            if X /= Lp1'(1) then
               Failed ("WRONG DEFAULT VALUE - LP1");
            end if;
         end G1;

         procedure G2 (X : Lp2 := F2) is
         begin
            if X /= Lp2'(2, 3) then
               Failed ("WRONG DEFAULT VALUE - LP2");
            end if;
         end G2;

         procedure G3 (X : Lp3 := F3) is
         begin
            if X /= Lp3'((4, 5), (6, 7)) then
               Failed ("WRONG DEFAULT VALUE - LP3");
            end if;
         end G3;

      begin

         G1;            -- LEGAL, DEFAULT USED
         G2;            -- LEGAL, DEFAULT USED
         G3;            -- LEGAL, DEFAULT USED

         G1 (F1);        -- LEGAL
         G2 (F2);        -- LEGAL
         G3 (F3);        -- LEGAL

      end Pack1;

      use Pack1;

      procedure G4 (X : Lp1 := F1) is
      begin
         G1;            -- LEGAL, DEFAULT USED
         G1 (X);
      end G4;

      procedure G5 (X : Lp2 := F2) is
      begin
         G2;            -- LEGAL, DEFAULT USED
         G2 (X);
      end G5;

      procedure G6 (X : Lp3 := F3) is
      begin
         G3;            -- DEFAULT USED
         G3 (X);
      end G6;

   begin

      G4;                 -- LEGAL, DEFAULT USED
      G5;                 -- LEGAL, DEFAULT USED
      G6;                 -- LEGAL, DEFAULT USED

      G4 (F1);             -- LEGAL
      G5 (F2);             -- LEGAL
      G6 (F3);             -- LEGAL

   end;

   Result;

end C74402a;
