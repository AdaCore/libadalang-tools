-- C42007E.ADA

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
-- CHECK THAT THE BOUNDS OF A STRING LITERAL ARE DETERMINED CORRECTLY.
-- IN PARTICULAR, CHECK THAT THE LOWER BOUND IS GIVEN BY 'FIRST OF THE
-- INDEX SUBTYPE WHEN THE STRING LITERAL IS USED AS:

--   E) THE LEFT OR RIGHT OPERAND OF "&".

-- TBN  7/28/86

with Report; use Report;
procedure C42007e is

begin

   Test
     ("C42007E",
      "CHECK THE BOUNDS OF A STRING LITERAL WHEN USED " &
      "AS THE LEFT OR RIGHT OPERAND OF THE CATENATION " &
      "OPERATOR");

   begin

      Case_E : declare

         subtype Str_Range is Integer range 2 .. 10;
         type Str is array (Str_Range range <>) of Character;

         function Concat1 return Str is
         begin
            return ("ABC" & (7 .. 8 => 'D'));
         end Concat1;

         function Concat2 return Str is
         begin
            return ((Ident_Int (4) .. 3 => 'A') & "BC");
         end Concat2;

         function Concat3 return String is
         begin
            return ("TEST" & (7 .. 8 => 'X'));
         end Concat3;

         function Concat4 return String is
         begin
            return ((8 .. 5 => 'A') & "DE");
         end Concat4;

      begin

         if Concat1'First /= Ident_Int (2) then
            Failed ("LOWER BOUND INCORRECTLY DETERMINED - 1");
         end if;
         if Concat1'Last /= 6 then
            Failed ("UPPER BOUND INCORRECTLY DETERMINED - 1");
         end if;
         if Concat1 /= "ABCDD" then
            Failed ("STRING INCORRECTLY DETERMINED - 1");
         end if;

         if Concat2'First /= Ident_Int (2) then
            Failed ("LOWER BOUND INCORRECTLY DETERMINED - 2");
         end if;
         if Concat2'Last /= 3 then
            Failed ("UPPER BOUND INCORRECTLY DETERMINED - 2");
         end if;
         if Concat2 /= "BC" then
            Failed ("STRING INCORRECTLY DETERMINED - 2");
         end if;

         if Concat3'First /= Ident_Int (1) then
            Failed ("LOWER BOUND INCORRECTLY DETERMINED - 3");
         end if;
         if Concat3'Last /= 6 then
            Failed ("UPPER BOUND INCORRECTLY DETERMINED - 3");
         end if;
         if Concat3 /= "TESTXX" then
            Failed ("STRING INCORRECTLY DETERMINED - 3");
         end if;

         if Concat4'First /= Ident_Int (1) then
            Failed ("LOWER BOUND INCORRECTLY DETERMINED - 4");
         end if;
         if Concat4'Last /= 2 then
            Failed ("UPPER BOUND INCORRECTLY DETERMINED - 4");
         end if;
         if Concat4 /= "DE" then
            Failed ("STRING INCORRECTLY DETERMINED - 4");
         end if;

      end Case_E;

   end;

   Result;

end C42007e;
