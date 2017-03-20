-- CC1301A.ADA

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
-- CHECK THAT DEFAULT GENERIC SUBPROGRAM PARAMETERS WORK CORRECTLY,
-- INCLUDING OVERLOADED AND PREDEFINED OPERATOR_SYMBOLS,
-- AND SUBPROGRAMS HIDDEN AT THE INSTANTIATION.
-- BOTH KINDS OF DEFAULTS ARE TESTED, FOR BOTH PROCEDURES
-- AND FUNCTIONS.

-- DAT 8/14/81
-- JBG 5/5/83
-- JBG 8/3/83

with Report; use Report;

procedure Cc1301a is

   function "-" (R, S : Integer) return Integer;

   function Next (X : Integer) return Integer;

   procedure Bump (X : in out Integer);

   generic
      with function "*" (A, B : Integer) return Integer is "-";
      with function "+" (R, S : Integer) return Integer is Standard."+";
      with function "-" (A, B : Integer) return Integer is <>;
      with function Nexto (Q : Integer) return Integer is Next;
      with procedure Bumpo (A : in out Integer) is Bump;
      with function Next (Q : Integer) return Integer is <>;
      with procedure Bump (Q : in out Integer) is <>;
      type Integer is range <>;
      with function "*" (A, B : Integer) return Integer is <>;
      with function "-" (A, B : Integer) return Integer is <>;
      with function Next (Q : Integer) return Integer is <>;
      with procedure Bump (Z : in out Integer) is <>;
   package Pkg is
      subtype Int is Standard.Integer;
      Diff : Int := -999;
   end Pkg;

   type Newint is new Integer range -1_000 .. 1_000;

   function Plus (Q1, Q2 : Integer) return Integer renames "+";

   function "+" (X, Y : Integer) return Integer is
   begin
      return Plus (X, Plus (Y, -10));
      -- (X + Y - 10)
   end "+";

   function "-" (R, S : Integer) return Integer is
   begin
      return -R + S;
      -- (-R + S - 10)
   end "-";

   function Next (X : Integer) return Integer is
   begin
      return X + 1;
      -- (X + 1 - 10)
      -- (X - 9)
   end Next;

   procedure Bump (X : in out Integer) is
   begin
      X := Next (X);
      -- (X := X - 9)
   end Bump;

   package body Pkg is
      W  : Integer;
      Wi : Int;
   begin
      W := Next (Integer'(3) * 4 - 2);
      -- (W := (4 ** 3 - 2) - 1)
      -- (W := 61)
      Bump (W);
      -- (W := 61 + 7)
      -- (W := 68)
      Wi := Next (Int'(3) * 4 - 2 + Nexto (0));
      -- (3 * 4) => (3 - 4) => (-3 + 4 - 10) = -9
      -- ((-9) - 2) => (2 + 2 - (-9) - 20) = -7
      -- (-7 + (-9)) => -16
      -- (WI := 7 - (-16)) => (WI := 23)
      Bumpo (Wi);
      -- (WI := 23 - 9) (= 14)
      Bump (Wi);
      -- (WI := 14 - 9) (= 5)
      Diff := Standard."-" (Int (W), Wi);
      -- (DIFF := 68 - 5) (= 63)
   end Pkg;

   function "*" (Y, X : Newint) return Newint is
   begin
      return X**Integer (Y);
      -- (X,Y) (Y ** X)
   end "*";

   function Next (Z : Newint) return Newint is
   begin
      return Z - 1;
      -- (Z - 1)
   end Next;

   procedure Bump (Zz : in out Newint) is
   begin
      Failed ("WRONG PROCEDURE CALLED");
   end Bump;
begin
   Test ("CC1301A", "DEFAULT GENERIC SUBPROGRAM PARAMETERS");

   declare
      procedure Bump (Qqq : in out Newint) is
      begin
         Qqq := Qqq + 7;
         -- (QQQ + 7)
      end Bump;

      function Next (Q7 : Integer) return Integer is
      begin
         return Q7 - 17;
         -- (-Q7 + 17 - 10)
         -- (7 - Q7)
      end Next;

      function "-" (Q3, Q4 : Integer) return Integer is
      begin
         return -Q3 + Q4 + Q4;
         -- (-Q3 + Q4 - 10 + Q4 - 10) = (Q4 + Q4 - Q3 - 20)
      end "-";

      package P1 is new Pkg (Integer => Newint);

   begin
      if P1.Diff /= 63 then
         Failed ("WRONG DEFAULT SUBPROGRAM PARAMETERS");
      end if;
   end;

   Result;
end Cc1301a;
