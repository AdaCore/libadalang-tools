-- C43107A.ADA

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
-- CHECK THAT AN EXPRESSION ASSOCIATED WITH MORE THAN ONE RECORD
-- COMPONENT IS EVALUATED ONCE FOR EACH ASSOCIATED COMPONENT.

-- EG  02/14/84

with Report;

procedure C43107a is

   use Report;

begin

   Test
     ("C43107A",
      "CHECK THAT AN EXPRESSION WITH MORE THAN ONE " &
      "RECORD COMPONENT IS EVALUATED ONCE FOR EACH " &
      "ASSOCIATED COMPONENT");

   begin

      Case_A : declare

         type T1 is array (1 .. 2) of Integer;
         type R1 is record
            A : T1;
            B : Integer;
            C : T1;
            D : Integer;
            E : Integer;
         end record;

         A1   : R1;
         Cntr : Integer := 0;

         function Fun1 (A : T1) return T1 is
         begin
            Cntr := Ident_Int (Cntr + 1);
            return A;
         end Fun1;

         function Fun2 (A : Integer) return Integer is
         begin
            Cntr := Cntr + 1;
            return Ident_Int (A);
         end Fun2;

      begin

         A1 := (A | C => Fun1 ((-1, -2)), others => Fun2 (-3) + 1);
         if Cntr /= 5 then
            Failed
              ("CASE A : INCORRECT NUMBER OF EVALUATIONS" &
               " OF RECORD ASSOCIATED COMPONENTS");
         end if;
         if A1.A /= (-1, -2) or
           A1.C /= (-1, -2) or
           A1.B /= -2 or
           A1.D /= -2 or
           A1.E /= -2
         then
            Failed ("CASE A : INCORRECT VALUES IN RECORD");
         end if;

      end Case_A;

      Case_B : declare

         type T2 is access Integer;
         type R2 is record
            A : T2;
            B : Integer;
            C : T2;
            D : Integer;
            E : Integer;
         end record;

         B1   : R2;
         Cntr : Integer := 0;

         function Fun3 return Integer is
         begin
            Cntr := Cntr + 1;
            return Ident_Int (2);
         end Fun3;

      begin

         B1 := (A | C => new Integer'(-1), B | D | E => Fun3);
         if B1.A = B1.C or Cntr /= 3 then
            Failed
              ("CASE B : INCORRECT NUMBER OF EVALUATION" &
               " OF RECORD ASSOCIATED COMPONENTS");
         end if;
         if B1.B /= 2 or
           B1.D /= 2 or
           B1.E /= 2 or
           B1.A = null or
           B1.C = null or
           B1.A = B1.C
         then
            Failed ("CASE B : INCORRECT VALUES IN RECORD");
         end if;

      end Case_B;

   end;

   Result;

end C43107a;
