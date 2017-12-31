-- C43205K.ADA

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

-- THE EXPRESSION OF AN ENCLOSING RECORD OR ARRAY AGGREGATE, AND THE EXPRESSION
-- GIVES THE VALUE OF A RECORD OR ARRAY COMPONENT (WHICH IS NECESSARILY
-- CONSTRAINED).

-- EG  01/27/84
-- JBG 3/30/84

with Report;

procedure C43205k is

   use Report;

begin

   Test
     ("C43205K",
      "THE EXPRESSION OF AN ENCLOSING RECORD " &
      "OR ARRAY AGGREGATE, AND THE EXPRESSION GIVES " &
      "THE VALUE OF A RECORD OR ARRAY COMPONENT");

   begin

      Case_K :
      begin

         Case_K1 :
         declare

            subtype Sk1 is Integer range 2 .. 6;
            type Base is array (Sk1 range <>) of Integer;
            subtype Te1 is Base (Ident_Int (3) .. 5);
            type Te2 is array (1 .. 2) of Te1;

            E1 : Te2;

         begin

            E1 := (1 .. 2 => (3, 2, 1));
            if (E1'First /= 1 or E1'Last /= 2)
              or else
              (E1 (1)'First /= 3 or E1 (1)'Last /= 5 or E1 (2)'First /= 3 or
               E1 (2)'Last /= 5)
            then
               Failed ("CASE K1 : INCORRECT BOUNDS");
            else
               if E1 /= (1 .. 2 => (3, 2, 1)) then
                  Failed
                    ("CASE K1 : ARRAY DOES NOT " &
                     "CONTAIN THE CORRECT VALUES");
               end if;
            end if;

         end Case_K1;

         Case_K2 :
         declare

            type Sk2 is range 2 .. 6;
            type Base is array (Sk2 range <>) of Integer;
            subtype Te1 is Base (3 .. 5);
            type Ter is record
               Rec : Te1;
            end record;

            E2 : Ter;

         begin

            E2 := (Rec => (3, 2, 1));
            if E2.Rec'First /= 3 or E2.Rec'Last /= 5 then
               Failed ("CASE K2 : INCORRECT BOUNDS");
            else
               if E2.Rec /= (3, 2, 1) then
                  Failed
                    ("CASE K2 : ARRAY DOES NOT " & "CONTAIN CORRECT VALUES");
               end if;
            end if;

         end Case_K2;

      end Case_K;

   end;

   Result;

end C43205k;
