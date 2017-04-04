-- C43205B.ADA

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
-- IN PARTICULAR, CHECK THAT THE LOWER BOUND IS GIVEN BY 'FIRST OF THE INDEX
-- SUBTYPE WHEN THE POSITIONAL AGGREGATE IS USED AS:

--   B) AN ACTUAL PARAMETER IN A GENERIC INSTANTIATION, AND THE FORMAL
--      PARAMETER IS UNCONSTRAINED.

-- EG  01/26/84

with Report;

procedure C43205b is

   use Report;

begin

   Test
     ("C43205B",
      "CASE B : UNCONSTRAINED ARRAY FORMAL GENERIC " & "PARAMETER");

   begin

      Case_B : declare

         subtype Stb is Integer range Ident_Int (-8) .. -5;
         type Tb is array (Stb range <>) of Integer;

         generic
            B1 : Tb;
         procedure Proc1;

         procedure Proc1 is
         begin
            if B1'First /= -8 then
               Failed
                 ("CASE B : LOWER BOUND INCORRECTLY " & "GIVEN BY 'FIRST");
            elsif B1'Last /= Ident_Int (-5) then
               Failed ("CASE B : UPPER BOUND INCORRECTLY " & "GIVEN BY 'LAST");
            elsif B1 /= (7, 6, 5, 4) then
               Failed
                 ("CASE B : ARRAY DOES NOT " & "CONTAIN THE CORRECT VALUES");
            end if;
         end Proc1;

         procedure Proc2 is new Proc1 ((7, 6, Ident_Int (5), 4));

      begin

         Proc2;

      end Case_B;

   end;

   Result;

end C43205b;
