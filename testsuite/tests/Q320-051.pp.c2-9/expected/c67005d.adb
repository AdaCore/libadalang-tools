-- C67005D.ADA

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
-- CHECK THAT EQUALITY CAN BE REDEFINED FOR AN ARBITRARY TYPE BY USING A
-- SEQUENCE OF RENAMING DECLARATIONS.

-- JBG 9/11/84

with Report; use Report;
procedure C67005d is

   function My_Equals (L, R : Integer) return Boolean is
   begin
      return False;
   end My_Equals;

   generic
      type Lp is limited private;
      with function "=" (L, R : Lp) return Boolean;
   package Equality_Operator is
      package Inner is
         function "=" (L, R : Lp) return Boolean renames Equality_Operator."=";
      end Inner;
   end Equality_Operator;

begin
   Test ("C67005D", "CHECK REDEFINITION OF ""="" BY RENAMING");

   declare

      Chk1 : Boolean := 3 = Ident_Int (3);     -- PREDEFINED "="

      -- REDEFINE INTEGER "=".

      package Int_Equality is new Equality_Operator (Integer, My_Equals);
      function "="
        (L, R : Integer) return Boolean renames
        Int_Equality.Inner."=";

      Chk2 : Boolean := 3 = Ident_Int (3);     -- REDEFINED "=".

   begin

      if not Chk1 then
         Failed ("PREDEFINED ""="" NOT USED");
      end if;

      if Chk2 then
         Failed ("REDEFINED ""="" NOT USED");
      end if;

   end;

   Result;

end C67005d;
