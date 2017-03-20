-- CC1304B.ADA

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
--     CHECK THAT GENERIC FORMAL SUBPROGRAMS MAY HAVE A PARAMETER
--     OF A GENERIC FORMAL TYPE, AND MAY RETURN A GENERIC FORMAL
--     TYPE.  CHECK MODES IN OUT AND OUT.

-- HISTORY:
--     BCB 08/04/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure Cc1304b is

begin
   Test
     ("CC1304B",
      "GENERIC FORMAL SUBPROGRAMS MAY HAVE A " &
      "PARAMETER OF A GENERIC FORMAL TYPE, AND MAY  " &
      "RETURN A GENERIC FORMAL TYPE.  CHECK MODES IN " &
      "OUT AND OUT");

   declare
      generic
         type T is (<>);
         with procedure S (P : out T);
         with procedure P (P : in out T);
         with function L return T;
      procedure Pr (Parm1, Parm2, Parm3 : in out T);

      procedure Pr (Parm1, Parm2, Parm3 : in out T) is
      begin
         S (P => Parm1);
         P (P => Parm2);
         Parm3 := L;
      end Pr;
   begin
      declare
         C  : Character := 'A';
         C1 : Character := 'Y';
         C2 : Character := 'I';
         B  : Boolean   := False;
         B1 : Boolean   := True;
         B2 : Boolean   := False;
         I  : Integer   := 5;
         I1 : Integer   := 10;
         I2 : Integer   := 0;
         type Enum is (E1, E2, E3);
         F  : Enum := E2;
         F1 : Enum := E1;
         F2 : Enum := E2;

         procedure Fc (P : out Character) is
         begin
            P := 'B';
         end Fc;

         procedure Fb (P : out Boolean) is
         begin
            P := not B;
         end Fb;

         procedure Fi (P : out Integer) is
         begin
            P := I + 1;
         end Fi;

         procedure Fe (P : out Enum) is
         begin
            P := Enum'Succ (F);
         end Fe;

         procedure Pc (P : in out Character) is
         begin
            P := 'Z';
         end Pc;

         procedure Pb (P : in out Boolean) is
         begin
            P := not B1;
         end Pb;

         procedure Pi (P : in out Integer) is
         begin
            P := I1 + 1;
         end Pi;

         procedure Pe (P : in out Enum) is
         begin
            P := Enum'Succ (F1);
         end Pe;

         function Lc return Character is
         begin
            return 'J';
         end Lc;

         function Lb return Boolean is
         begin
            return True;
         end Lb;

         function Li return Integer is
         begin
            return Ident_Int (5);
         end Li;

         function Le return Enum is
         begin
            return Enum'Succ (F2);
         end Le;

         package Pkg2 is
            procedure P1 is new Pr (Character, Fc, Pc, Lc);
            procedure P2 is new Pr (Boolean, Fb, Pb, Lb);
            procedure P3 is new Pr (Integer, Fi, Pi, Li);
            procedure P4 is new Pr (Enum, Fe, Pe, Le);
         end Pkg2;

         package body Pkg2 is
         begin
            P1 (C, C1, C2);
            P2 (B, B1, B2);
            P3 (I, I1, I2);
            P4 (F, F1, F2);
         end Pkg2;
      begin
         if C /= 'B' or B /= True or I /= 6 or F /= E3 then
            Failed ("SUBPROGRAM PARAMETERS OF FORMAL TYPES - " & "MODE OUT");
         end if;

         if C1 /= 'Z' or B1 /= False or I1 /= 11 or F1 /= E2 then
            Failed
              ("SUBPROGRAM PARAMETERS OF FORMAL TYPES - " & "MODE IN OUT");
         end if;

         if C2 /= 'J' or B2 /= True or I2 /= 5 or F2 /= E3 then
            Failed
              ("GENERIC FORMAL SUBPROGRAMS RETURNING A " &
               "GENERIC FORMAL TYPE");
         end if;
      end;
   end;

   Result;
end Cc1304b;
