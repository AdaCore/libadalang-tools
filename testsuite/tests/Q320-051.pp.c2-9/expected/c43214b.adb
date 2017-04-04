-- C43214B.ADA

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

procedure C43214b is

   use Report;

begin

   Test ("C43214B", "SUBPROGRAM WITH CONSTRAINED ARRAY FORMAL " & "PARAMETER");

   begin

      Case_A : begin

--             COMMENT ("CASE A1 : SUBPROGRAM WITH CONSTRAINED " &
--                      "ONE-DIMENSIONAL ARRAY FORMAL PARAMETER");

         Case_A1 : declare

            subtype Sta1 is String (Ident_Int (11) .. 15);

            procedure Proc1 (A : Sta1) is
            begin
               if A'First /= 11 then
                  Failed ("CASE 1 : LOWER BOUND " & "INCORRECT");
               elsif A'Last /= 15 then
                  Failed ("CASE 1 : UPPER BOUND " & "INCORRECT");
               elsif A /= "ABCDE" then
                  Failed
                    ("CASE 1 : ARRAY DOES NOT " &
                     "CONTAIN THE CORRECT VALUES");
               end if;
            end Proc1;

         begin

            Proc1 ("ABCDE");

         end Case_A1;

--             COMMENT ("CASE A2 : SUBPROGRAM WITH CONSTRAINED " &
--                      "TWO-DIMENSIONAL ARRAY FORMAL PARAMETER");

         Case_A2 : declare

            type Ta is array (11 .. 12, 10 .. 11) of Character;

            procedure Proc1 (A : Ta) is
            begin
               if A'First (1) /= 11 or A'First (2) /= 10 then
                  Failed ("CASE 2 : LOWER BOUND " & "INCORRECT");
               elsif A'Last (1) /= 12 or A'Last (2) /= 11 then
                  Failed ("CASE 2 : UPPER BOUND " & "INCORRECT");
               elsif A /= ("AB", "CD") then
                  Failed
                    ("CASE 2 : ARRAY DOES NOT " &
                     "CONTAIN THE CORRECT VALUES");
               end if;
            end Proc1;

         begin

            Proc1 (("AB", "CD"));

         end Case_A2;

      end Case_A;

   end;

   Result;

end C43214b;
