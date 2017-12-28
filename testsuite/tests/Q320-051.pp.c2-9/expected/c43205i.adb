-- C43205I.ADA

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
-- CHECK THAT THE BOUNDS OF A POSITIONAL AGGREGATE ARE DETERMINED CORRECTLY.
-- IN PARTICULAR, CHECK THAT THE LOWER BOUND IS GIVEN BY THE LOWER BOUND OF
-- THE APPLICABLE INDEX CONSTRAINT WHEN THE POSITIONAL AGGREGATE IS USED AS:

-- THE RETURN EXPRESSION IN A FUNCTION WHOSE RETURN TYPE IS CONSTRAINED.

-- EG  01/27/84

with Report;

procedure C43205i is

   use Report;

begin

   Test ("C43205I", "CONSTRAINED FUNCTION RESULT TYPE");

   begin

      Case_I :
      declare

         subtype Stc is Integer range -2 .. 10;
         type Base is array (Stc range <>, Stc range <>) of Integer;
         subtype Tc is Base (Ident_Int (-1) .. 0, 7 .. 9);

         function Fun1 (A : Integer) return Tc is
         begin
            return ((5, 4, 3), (2, 1, 0));
         end Fun1;

      begin

         if Fun1 (5)'First (1) /= -1 then
            Failed ("CASE I : LOWER BOUND INCORRECT " & "FOR 'FIRST(1)");
         elsif Fun1 (5)'First (2) /= 7 then
            Failed ("CASE I : LOWER BOUND INCORRECT " & "FOR 'FIRST(2)");
         elsif Fun1 (5)'Last (1) /= 0 then
            Failed ("CASE I : UPPER BOUND INCORRECT " & "FOR 'LAST(1)");
         elsif Fun1 (5)'Last (2) /= 9 then
            Failed ("CASE I : UPPER BOUND INCORRECT " & "FOR 'LAST(2)");
         elsif Fun1 (5) /= ((5, 4, 3), (2, 1, 0)) then
            Failed
              ("CASE I : FUNCTION DOES NOT " & "RETURN THE CORRECT VALUES");
         end if;

      end Case_I;

   end;

   Result;

end C43205i;
