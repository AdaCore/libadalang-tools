-- CC3007A.ADA

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
-- CHECK THAT NAMES IN A GENERIC DECLARATIONS ARE STATICALLY BOUND.

-- DAT 9/18/81
-- SPS 2/7/83

with Report; use Report;

procedure Cc3007a is
begin
   Test ("CC3007A", "NAMES IN GENERICS ARE STATICALLY BOUND");

   declare
      I : Integer := 1;
      Ex : exception;
      Ia : Integer := I'Size;

      function F (X : Integer) return Integer;

      package P is
         Q : Integer := 1;
      end P;

      generic
         J : in out Integer;
         with function Fp (X : Integer) return Integer is F;
      package Gp is
         V1 : Integer := F (I);
         V2 : Integer := Fp (I);
      end Gp;

      generic
         type T is range <>;
         with function F1 (X : Integer) return Integer is F;
         Inp : in T := T (I'Size);
      function F1 (X : T) return T;

      function F1 (X : T) return T is
      begin
         if Inp /= T (Ia) then
            Failed ("INCORRECT GENERIC BINDING 2");
         end if;
         I := I + 1;
         return 2 * T (F1 (F (Integer (X) + I + P.Q)));
      end F1;

      package body Gp is
         package P is
            Q : Integer := I + 1;
         end P;
         I : Integer := 1_000;
         function F is new F1 (Integer);
         function F2 is new F1 (Integer);
      begin
         P.Q := F2 (J + P.Q + V1 + 2 * V2);
         J   := P.Q;
         raise Ex;
      end Gp;

      function F (X : Integer) return Integer is
      begin
         I := I + 2;
         return X + I;
      end F;
   begin
      declare
         I : Integer := 1_000;
         Ex : exception;
         function F is new F1 (Integer);
         V : Integer := F (3);
      begin
         begin
            declare
               package P is new Gp (V);
            begin
               Failed ("EX NOT RAISED");
            end;
         exception
            when Ex =>
               Failed ("WRONG EXCEPTION RAISED");
            when others =>
               if V /= 266 then
                  Failed ("WRONG BINDING IN GENERICS");
               end if;
               raise;
         end;

      end;
   exception
      when Ex =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 2");
   end;

   Result;
end Cc3007a;
