-- CC3606A.ADA

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
--     CHECK THAT THE DEFAULT EXPRESSIONS OF A FORMAL SUBPROGRAM'S
--     FORMAL PARAMETERS ARE USED WHEN THE FORMAL SUBPROGRAM IS
--     CALLED IN THE INSTANTIATED UNIT (RATHER THAN ANY DEFAULT
--     ASSOCIATED WITH ACTUAL SUBPROGRAM'S PARAMETERS).

-- HISTORY:
--     BCB 09/29/87  CREATED ORIGINAL TEST.

with Report; use Report;

procedure Cc3606a is

   X : Boolean;
   Y : Boolean;

   function Func (A : Integer := 35) return Boolean is
   begin
      return (A = 7);
   end Func;

   procedure Proc (B : Integer := 35) is
   begin
      if B /= 7 then
         Failed
           ("DEFAULT EXPRESSION OF FORMAL PARAMETER " &
            "PROCEDURE NOT USED - 1");
      end if;
   end Proc;

   function Func1 (C : Integer := 35) return Boolean is
   begin
      return (C = 7);
   end Func1;

   procedure Proc3 (D : Integer := 35) is
   begin
      if D /= 7 then
         Failed
           ("DEFAULT EXPRESSION OF FORMAL PARAMETER " &
            "PROCEDURE NOT USED - 2");
      end if;
   end Proc3;

   generic
      with function Func (A : Integer := 7) return Boolean;
   function Genfunc return Boolean;

   function Genfunc return Boolean is
   begin
      if not Func then
         Failed
           ("DEFAULT EXPRESSION OF FORMAL PARAMETER " &
            "FUNCTION NOT USED - 1");
      end if;
      return True;
   end Genfunc;

   generic
      with procedure Proc (B : Integer := 7);
   package Pkg is
   end Pkg;

   package body Pkg is
   begin
      Proc;
   end Pkg;

   generic
      with function Func1 (C : Integer := 7) return Boolean;
   procedure Proc2;

   procedure Proc2 is
   begin
      if not Func1 then
         Failed
           ("DEFAULT EXPRESSION OF FORMAL PARAMETER " &
            "FUNCTION NOT USED - 2");
      end if;
   end Proc2;

   generic
      with procedure Proc3 (D : Integer := 7) is <>;
   function Genfunc1 return Boolean;

   function Genfunc1 return Boolean is
   begin
      Proc3;
      return True;
   end Genfunc1;

   function Newfunc is new Genfunc (Func);

   package Pack is new Pkg (Proc);

   procedure Proc4 is new Proc2 (Func1);

   function Newfunc1 is new Genfunc1;

begin

   Test
     ("CC3606A",
      "CHECK THAT THE DEFAULT EXPRESSIONS OF A " &
      "FORMAL SUBPROGRAM'S FORMAL PARAMETERS ARE " &
      "USED WHEN THE FORMAL SUBPROGRAM IS CALLED IN " &
      "THE INSTANTIATED UNIT (RATHER THAN ANY " &
      "DEFAULT ASSOCIATED WITH ACTUAL SUBPROGRAM'S " &
      "PARAMETERS)");

   X := Newfunc;
   Y := Newfunc1;
   Proc4;

   Result;
end Cc3606a;
