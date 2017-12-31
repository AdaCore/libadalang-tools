-- CC3120A.ADA

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
-- CHECK THAT GENERIC IN PARAMETERS ARE ALWAYS COPIED, AND THAT GENERIC IN OUT
-- PARAMETERS ARE ALWAYS RENAMED.

-- DAT 8/10/81
-- SPS 10/21/82

with Report; use Report;

procedure Cc3120a is
begin
   Test
     ("CC3120A",
      "GENERIC IN PARMS ARE COPIED, GENERIC IN OUT" & " PARMS ARE RENAMED");

   declare
      S1, S2     : Integer;
      A1, A2, A3 : String (1 .. Ident_Int (3));

      type Rec is record
         C1, C2 : Integer := 1;
      end record;

      R1, R2 : Rec;

      package P is
         type Priv is private;
         procedure Set_Priv (P : in out Priv);
      private
         type Priv is new Rec;
      end P;
      use P;

      P1, P2 : Priv;
      Ex : exception;

      generic
         type T is private;
         P1 : in out T;
         P2 : in T;
      procedure Gp;

      B_Arr : array (1 .. 10) of Boolean;

      package body P is
         procedure Set_Priv (P : in out Priv) is
         begin
            P.C1 := 3;
         end Set_Priv;
      end P;

      procedure Gp is
      begin
         if P1 = P2 then
            Failed ("PARAMETER SCREW_UP SOMEWHERE");
         end if;
         P1 := P2;
         if P1 /= P2 then
            Failed ("ASSIGNMENT SCREW_UP SOMEWHERE");
         end if;
         raise Ex;
         Failed ("RAISE STATEMENT DOESN'T WORK");
      end Gp;
   begin
      S1    := 4;
      S2    := 5;
      A1    := "XYZ";
      A2    := "ABC";
      A3    := "DEF";
      R1.C1 := 4;
      R2.C1 := 5;
      B_Arr := (1 | 3 | 5 | 7 | 9 => True, 2 | 4 | 6 | 8 | 10 => False);
      Set_Priv (P2);

      if S1 = S2 or A1 = A3 or R1 = R2 or P1 = P2 then
         Failed ("WRONG ASSIGNMENT");
      end if;
      begin
         declare
            procedure Pr is new Gp (Integer, S1, S2);
         begin
            S2 := S1;
            Pr;       -- OLD S2 ASSIGNED TO S1, SO S1 /= S2 NOW
            Failed ("EX NOT RAISED 1");
         exception
            when Ex =>
               null;
         end;

         declare
            subtype Str_1_3 is String (Ident_Int (1) .. 3);
            procedure Pr is new Gp (Str_1_3, A1, A3);
         begin
            A3 := A1;
            Pr;
            Failed ("EX NOT RAISED 2");
         exception
            when Ex =>
               null;
         end;

         declare
            procedure Pr is new Gp (Rec, R1, R2);
         begin
            R2 := R1;
            Pr;
            Failed ("EX NOT RAISED 3");
         exception
            when Ex =>
               null;
         end;

         declare
            procedure Pr is new Gp (Priv, P1, P2);
         begin
            P2 := P1;
            Pr;
            Failed ("EX NOT RAISED 4");
         exception
            when Ex =>
               null;
         end;
         declare
            procedure Pr is new Gp (Character, A3 (Ident_Int (2)),
               A3 (Ident_Int (3)));
         begin
            A3 (3) := A3 (2);
            Pr;
            Failed ("EX NOT RAISED 5");
         exception
            when Ex =>
               null;
         end;

         declare
            procedure Pr is new Gp (Boolean, B_Arr (Ident_Int (2)),
               B_Arr (Ident_Int (3)));
         begin
            B_Arr (3) := B_Arr (2);
            Pr;
            Failed ("EX NOT RAISED 6");
         exception
            when Ex =>
               null;
         end;
      end;

      if S1 = S2 or A1 = A2 or R1 = R2 or P1 = P2 or A3 (2) = A3 (3) or
        B_Arr (2) = B_Arr (3) then
         Failed ("ASSIGNMENT FAILED 2");
      end if;
   end;

   Result;
end Cc3120a;
