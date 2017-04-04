-- C74401D.ADA

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
-- CHECK THAT AN OUT PARAMETER HAVING A LIMITED TYPE IS ALLOWED FOR FORMAL
-- SUBPROGRAM PARAMETERS. (ONLY THE CASE OF PRACTICAL INTEREST, NAMELY,
-- LIMITED PRIVATE TYPES, IS CHECKED HERE.)

-- CHECK THAT AN OUT PARAMETER IN A RENAMING DECLARATION CAN HAVE A LIMITED
-- PRIVATE TYPE WHEN IT RENAMES A GENERIC FORMAL SUBPROGRAM.

-- JBG 5/1/85

with Report; use Report;
procedure C74401d is

   package P is
      type Lp is limited private;
      procedure P1 (X : out Lp);
      procedure P2 (X : out Lp);
      function Eq (L, R : Lp) return Boolean;
      Val1 : constant Lp;
      Val2 : constant Lp;
   private
      type Lp is new Integer;
      Val1 : constant Lp := Lp (Ident_Int (3));
      Val2 : constant Lp := Lp (Ident_Int (-3));
   end P;

   package body P is
      procedure P1 (X : out Lp) is
      begin
         X := 3;
      end P1;

      procedure P2 (X : out Lp) is
      begin
         X := -3;
      end P2;

      function Eq (L, R : Lp) return Boolean is
      begin
         return L = R;
      end Eq;
   end P;

   generic
      with procedure P3 (Y : out P.Lp);
      type Glp is limited private;
      with procedure P4 (Y : out Glp);
      Val_P3 : in out P.Lp;
      Val_P4 : in out Glp;
   package Gpack is
      procedure Renamed (X : out Glp) renames P4;   -- OK. RENAMING.
   end Gpack;

   package body Gpack is
   begin
      P3 (Val_P3);
      P4 (Val_P4);
   end Gpack;

begin

   Test
     ("C74401D",
      "CHECK THAT GENERIC FORMAL SUBPROGRAMS CAN HAVE " &
      "LIMITED PRIVATE OUT PARAMETERS");

   declare
      Var1 : P.Lp;
      Var2 : P.Lp;
      package Pack is new Gpack (P.P1, P.Lp, P.P2, Var1, Var2);
   begin
      if not P.Eq (Var1, P.Val1) then
         Failed ("P1 INVOCATION INCORRECT");
      end if;

      if not P.Eq (Var2, P.Val2) then
         Failed ("P2 INVOCATION INCORRECT");
      end if;

      P.P1 (Var2);        -- RESET VALUE OF VAR2.
      Pack.Renamed (Var2);

      if not P.Eq (Var2, P.Val2) then
         Failed ("RENAMED INVOCATION INCORRECT");
      end if;
   end;

   Result;

end C74401d;
