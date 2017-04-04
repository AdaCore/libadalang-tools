-- C43205A.ADA

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

--   A) AN ACTUAL PARAMETER IN A SUBPROGRAM OR ENTRY CALL, AND THE
--      FORMAL PARAMETER IS UNCONSTRAINED.

-- EG  01/26/84

with Report;

procedure C43205a is

   use Report;

begin

   Test
     ("C43205A",
      "CASE A1 : SUBPROGRAM WITH UNCONSTRAINED " &
      "ONE-DIMENSIONAL ARRAY FORMAL PARAMETER");

   begin

      Case_A : begin

         Case_A1 : declare

            subtype Sta is Integer range 11 .. 15;
            type Ta is array (Sta range <>) of Integer;

            procedure Proc1 (A : Ta) is
            begin
               if A'First /= Ident_Int (11) then
                  Failed
                    ("CASE A1 : LOWER BOUND " & "INCORRECTLY GIVEN BY 'FIRST");
               elsif A'Last /= 15 then
                  Failed
                    ("CASE A1 : UPPER BOUND " & "INCORRECTLY GIVEN BY 'LAST");
               elsif A /= (6, 7, 8, 9, 10) then
                  Failed
                    ("CASE A1 : ARRAY DOES NOT " &
                     "CONTAIN THE CORRECT VALUES");
               end if;
            end Proc1;

         begin

            Proc1 ((6, 7, 8, 9, Ident_Int (10)));

         end Case_A1;

         Comment
           ("CASE A2 : SUBPROGRAM WITH UNCONSTRAINED " &
            "TWO-DIMENSIONAL ARRAY FORMAL PARAMETER");

         Case_A2 : declare

            subtype Sta1 is Integer range 11 .. Ident_Int (12);
            subtype Sta2 is Integer range 10 .. 11;
            type Ta is array (Sta1 range <>, Sta2 range <>) of Integer;

            procedure Proc1 (A : Ta) is
            begin
               if A'First (1) /= 11 or A'First (2) /= 10 then
                  Failed
                    ("CASE A2 : LOWER BOUND " & "INCORRECTLY GIVEN BY 'FIRST");
               elsif A'Last (1) /= 12 or A'Last (2) /= Ident_Int (11) then
                  Failed
                    ("CASE A2 : UPPER BOUND " & "INCORRECTLY GIVEN BY 'LAST");
               elsif A /= ((1, 2), (3, 4)) then
                  Failed
                    ("CASE A2 : ARRAY DOES NOT " &
                     "CONTAIN THE CORRECT VALUES");
               end if;
            end Proc1;

         begin

            Proc1 (((1, 2), (Ident_Int (3), 4)));

         end Case_A2;

      end Case_A;

   end;

   Result;

end C43205a;
