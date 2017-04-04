-- C43205G.ADA

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

-- AN ACTUAL PARAMETER IN A SUBPROGRAM, AND THE FORMAL PARAMETER IS
-- CONSTRAINED.

-- EG  01/27/84

with Report;

procedure C43205g is

   use Report;

begin

   Test
     ("C43205G",
      "SUBPROGRAM WITH CONSTRAINED " &
      "ONE-DIMENSIONAL ARRAY FORMAL PARAMETER");

   begin

      Case_G : begin

         Case_G1 : declare

            type Ta is array (Ident_Int (11) .. 15) of Integer;

            procedure Proc1 (A : Ta) is
            begin
               if A'First /= 11 then
                  Failed ("CASE A1 : LOWER BOUND " & "INCORRECT");
               elsif A'Last /= 15 then
                  Failed ("CASE A1 : UPPER BOUND " & "INCORRECT");
               elsif A /= (6, 7, 8, 9, 10) then
                  Failed
                    ("CASE A1 : ARRAY DOES NOT " &
                     "CONTAIN THE CORRECT VALUES");
               end if;
            end Proc1;

         begin

            Proc1 ((6, 7, 8, Ident_Int (9), 10));

         end Case_G1;

         Case_G2 : declare

            type Ta is array (11 .. 12, Ident_Int (10) .. 11) of Integer;

            procedure Proc1 (A : Ta) is
            begin
               if A'First (1) /= 11 or A'First (2) /= 10 then
                  Failed ("CASE A2 : LOWER BOUND " & "INCORRECT");
               elsif A'Last (1) /= 12 or A'Last (2) /= 11 then
                  Failed ("CASE A2 : UPPER BOUND " & "INCORRECT");
               elsif A /= ((1, 2), (3, 4)) then
                  Failed
                    ("CASE A2 : ARRAY DOES NOT " &
                     "CONTAIN THE CORRECT VALUES");
               end if;
            end Proc1;

         begin

            Proc1 (((1, 2), (3, 4)));

         end Case_G2;

      end Case_G;

   end;

   Result;

end C43205g;
