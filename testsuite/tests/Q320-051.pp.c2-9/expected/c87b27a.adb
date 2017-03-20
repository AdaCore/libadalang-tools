-- C87B27A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- THE TYPE OF A STRING LITERAL MUST BE DETERMINED FROM THE FACT
-- THAT A STRING LITERAL IS A VALUE OF A ONE DIMENSIONAL ARRAY OF
-- CHARACTER COMPONENTS.

-- TRH  18 AUG 82
-- DSJ  07 JUN 83

with Report; use Report;

procedure C87b27a is

   type Enumlit is (A, B, C, D, E, F);
   type New_Char is new Character range 'G' .. 'Z';
   type Chars3 is ('G', 'H', 'I', 'K', 'M', 'N', 'P', 'R', 'S', 'T');
   type Chars4 is ('S', 'T', 'R', 'I', 'N', 'G', 'Z', 'A', 'P');
   type New_Str is array (A .. F) of New_Char;
   type String3 is array (11 .. 16) of Chars3;
   type String4 is array (21 .. 26) of Chars4;
   type Enum_Vec is array (1 .. 6) of Enumlit;
   type Char_Grid is array (D .. F, 1 .. 3) of New_Char;
   type Str_List is array (1 .. 6) of String (1 .. 1);
   Err : Boolean := False;

   procedure P (X : New_Str) is
   begin
      null;
   end P;

   procedure P (X : Enum_Vec) is
   begin
      Err := True;
   end P;

   procedure P (X : Char_Grid) is
   begin
      Err := True;
   end P;

   procedure P (X : Str_List) is
   begin
      Err := True;
   end P;

begin
   Test ("C87B27A", "OVERLOADING RESOLUTION OF STRING LITERALS");

   P ("STRING");

   if Err then
      Failed ("RESOLUTION INCORRECT FOR STRING LITERALS");
   end if;

   Result;
end C87b27a;
