-- C43205D.ADA

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
-- CHECK THAT THE BOUNDS OF A POSITIONAL AGGREGATE ARE DETERMINED
-- CORRECTLY. IN PARTICULAR, CHECK THAT THE LOWER BOUND IS GIVEN BY
-- 'FIRST OF THE INDEX SUBTYPE WHEN THE POSITIONAL AGGREGATE IS USED AS:

--   D) THE INITIALIZATION EXPRESSION OF A CONSTANT WHOSE TYPE MARK
--      DENOTES AN UNCONSTRAINED ARRAY.

-- EG  01/26/84

with Report;

procedure C43205d is

   use Report;

begin

   Test
     ("C43205D",
      "CASE D : INITIALIZATION OF UNCONSTRAINED " & "ARRAY CONSTANT");

   begin

      Case_D : declare

         subtype Std is Integer range Ident_Int (11) .. 13;
         type Td is array (Std range <>) of Integer;

         D1 : constant Td := (-1, -2, -3);

      begin

         if D1'First /= 11 then
            Failed ("CASE D : LOWER BOUND INCORRECTLY " & "GIVEN BY 'FIRST");
         elsif D1'Last /= 13 then
            Failed ("CASE D : UPPER BOUND INCORRECTLY " & "GIVEN BY 'LAST");
         elsif D1 /= (-1, -2, -3) then
            Failed ("CASE D : ARRAY DOES NOT CONTAIN " & "THE CORRECT VALUES");
         end if;

      end Case_D;

   end;

   Result;

end C43205d;
