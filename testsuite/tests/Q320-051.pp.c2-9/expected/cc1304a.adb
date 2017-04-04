-- CC1304A.ADA

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
-- CHECK THAT GENERIC FORMAL SUBPROGRAMS MAY HAVE A PARAMETER OF A GENERIC
-- FORMAL TYPE, AND MAY RETURN A GENERIC FORMAL TYPE.

-- DAT 8/27/81

with Report; use Report;

procedure Cc1304a is
begin
   Test
     ("CC1304A",
      "GENERIC FORMAL SUBPROGRAMS MAY HAVE PARAMETERS" &
      " OF (AND RETURN) A FORMAL TYPE");

   declare
      generic
         type T is (<>);
         with function S (P : T) return T;
         with procedure P (P : T);
      procedure Pr (Parm : T);

      procedure Pr (Parm : T) is
      begin
         P (P => S (P => Parm));
      end Pr;
   begin
      declare
         C : Character := 'A';
         B : Boolean   := False;
         I : Integer   := 5;
         type Enum is (E1, E2, E3);
         E : Enum := E2;

         function Fc (P : Character) return Character is
         begin
            return 'B';
         end Fc;

         function Fb (P : Boolean) return Boolean is
         begin
            return not P;
         end Fb;

         function Fi (P : Integer) return Integer is
         begin
            return P + 1;
         end Fi;

         function Fe (P : Enum) return Enum is
         begin
            return Enum'Succ (P);
         end Fe;

         procedure Pc (P : Character) is
         begin
            C := P;
         end Pc;

         procedure Pb (P : Boolean) is
         begin
            B := P;
         end Pb;

         procedure Pi (P : Integer) is
         begin
            I := P;
         end Pi;

         procedure Pe (P : Enum) is
         begin
            E := P;
         end Pe;

         package Pkg2 is
            procedure P1 is new Pr (Character, Fc, Pc);
            procedure P2 is new Pr (Boolean, Fb, Pb);
            procedure P3 is new Pr (Integer, Fi, Pi);
            procedure P4 is new Pr (Enum, Fe, Pe);
         end Pkg2;

         package body Pkg2 is
         begin
            P1 (C);
            P2 (B);
            P3 (I);
            P4 (E);
         end Pkg2;
      begin
         if C /= 'B' or B /= True or I /= 6 or E /= E3 then
            Failed ("SUBPROGRAM PARAMETERS OF FORMAL TYPES");
         end if;
      end;
   end;

   Result;
end Cc1304a;
