-- C43214E.ADA

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
-- CHECK THAT THE LOWER BOUND FOR THE STRING LITERAL IS DETERMINED BY THE
-- APPLICABLE INDEX CONSTRAINT, WHEN ONE EXISTS.

-- EG  02/10/84

with Report;

procedure C43214e is

   use Report;

begin

   Test ("C43214E", "INITIALIZATION OF CONSTRAINED ARRAY");

   begin

      Case_D : begin

--             COMMENT ("CASE D1 : INITIALIZATION OF CONSTRAINED " &
--                      "ARRAY CONSTANT");

         Case_D1 : declare

            D1 : constant String (11 .. 13) := "ABC";

         begin

            if D1'First /= 11 then
               Failed ("CASE 1 : LOWER BOUND INCORRECT");
            elsif D1'Last /= 13 then
               Failed ("CASE 1 : UPPER BOUND INCORRECT");
            elsif D1 /= "ABC" then
               Failed
                 ("CASE 1 : ARRAY DOES NOT " & "CONTAIN THE CORRECT VALUES");
            end if;

         end Case_D1;

--             COMMENT ("CASE D2 : INITIALIZATION OF CONSTRAINED " &
--                      "ARRAY VARIABLE");

         Case_D2 : declare

            D2 : String (11 .. 13) := "ABC";

         begin

            if D2'First /= 11 then
               Failed ("CASE 2 : LOWER BOUND INCORRECT");
            elsif D2'Last /= 13 then
               Failed ("CASE 2 : UPPER BOUND INCORRECT");
            elsif D2 /= "ABC" then
               Failed ("CASE 2 : INCORRECT VALUES");
            end if;

         end Case_D2;

--             COMMENT ("CASE D3 : INITIALIZATION OF CONSTRAINED " &
--                      "ARRAY FORMAL PARAMETER OF A SUBPROGRAM");

         Case_D3 : declare

            subtype Std3 is String (Ident_Int (5) .. 7);

            procedure Proc1 (A : Std3 := "ABC") is
            begin
               if A'First /= 5 then
                  Failed ("CASE 3 : LOWER BOUND " & "INCORRECT");
               elsif A'Last /= 7 then
                  Failed ("CASE 3 : UPPER BOUND " & "INCORRECT");
               elsif A /= "ABC" then
                  Failed ("CASE 3 : INCORRECT VALUES");
               end if;
            end Proc1;

         begin

            Proc1;

         end Case_D3;

--             COMMENT ("CASE D4 : INITIALIZATION OF CONSTRAINED " &
--                      "ARRAY FORMAL PARAMETER OF A GENERIC UNIT");

         Case_D4 : declare

            subtype Std4 is String (5 .. 8);

            generic
               D4 : Std4 := "ABCD";
            procedure Proc1;

            procedure Proc1 is
            begin
               if D4'First /= 5 then
                  Failed ("CASE 4 : LOWER BOUND " & "INCORRECT");
               elsif D4'Last /= 8 then
                  Failed ("CASE 4 : UPPER BOUND " & "INCORRECT");
               elsif D4 /= "ABCD" then
                  Failed ("CASE 4 : INCORRECT VALUES");
               end if;
            end Proc1;

            procedure Proc2 is new Proc1;

         begin

            Proc2;

         end Case_D4;

      end Case_D;

   end;

   Result;

end C43214e;
