-- C43205J.ADA

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
-- THE LOWER BOUND OF THE APPLICABLE INDEX CONSTRAINT WHEN THE
-- POSITIONAL AGGREGATE IS USED AS:

--   J) THE INITIALIZATION EXPRESSION OF A CONSTANT, VARIABLE, OR FORMAL
--      PARAMETER (OF A SUBPROGRAM, ENTRY, OR GENERIC UNIT) WHEN THE
--      TYPE OF THE CONSTANT, VARIABLE, OR PARAMETER IS CONSTRAINED.

-- EG  01/27/84

with Report;

procedure C43205j is

   use Report;

begin

   Test ("C43205J", "CASE J : INITIALIZATION OF CONSTRAINED " & "ARRAY");

   begin

      Case_J : begin

         Case_J1 : declare

            type Td1 is array (Ident_Int (11) .. 13) of Integer;

            D1 : constant Td1 := (-1, -2, -3);

         begin

            if D1'First /= 11 then
               Failed ("CASE J1 : LOWER BOUND INCORRECT");
            elsif D1'Last /= 13 then
               Failed ("CASE J1 : UPPER BOUND INCORRECT");
            elsif D1 /= (-1, -2, -3) then
               Failed
                 ("CASE J1 : ARRAY DOES NOT " &
                  "CONTAINING THE CORRECT VALUES");
            end if;

         end Case_J1;

         Case_J2 : declare

            type Td2 is array (Integer range -13 .. -11) of Integer;
            D2 : Td2 := (3, 2, 1);

         begin

            if D2'First /= -13 then
               Failed ("CASE J2 : LOWER BOUND INCORRECT");
            elsif D2'Last /= -11 then
               Failed ("CASE J2 : UPPER BOUND INCORRECT");
            elsif D2 /= (3, 2, 1) then
               Failed ("CASE J2 : INCORRECT VALUES");
            end if;

         end Case_J2;

         Case_J3 : declare

            type Td3 is array (Ident_Int (5) .. 7) of Integer;

            procedure Proc1 (A : Td3 := (2, 3, 4)) is
            begin
               if A'First /= 5 then
                  Failed ("CASE J3 : LOWER BOUND " & "INCORRECT");
               elsif A'Last /= 7 then
                  Failed ("CASE J3 : UPPER BOUND " & "INCORRECT");
               elsif A /= (2, 3, 4) then
                  Failed ("CASE J3 : INCORRECT VALUES");
               end if;
            end Proc1;

         begin

            Proc1;

         end Case_J3;

         Case_J4 : declare

            type Td4 is array (5 .. 8) of Integer;

            generic
               D4 : Td4 := (1, -2, 3, -4);
            procedure Proc1;

            procedure Proc1 is
            begin
               if D4'First /= 5 then
                  Failed ("CASE J4 : LOWER BOUND " & "INCORRECT");
               elsif D4'Last /= 8 then
                  Failed ("CASE J4 : UPPER BOUND " & "INCORRECT");
               elsif D4 /= (1, -2, 3, -4) then
                  Failed ("CASE J4 : INCORRECT VALUES");
               end if;
            end Proc1;

            procedure Proc2 is new Proc1;

         begin

            Proc2;

         end Case_J4;

      end Case_J;

   end;

   Result;

end C43205j;
