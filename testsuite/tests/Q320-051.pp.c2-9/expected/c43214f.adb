-- C43214F.ADA

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
-- JBG 3/30/84

with Report;

procedure C43214f is

   use Report;

begin

   Test
     ("C43214F", "ARRAY COMPONENT EXPRESSION OF AN ENCLOSING " & "AGGREGATE");

   begin

      Case_E :
      begin

--             COMMENT ("CASE E1 : ARRAY COMPONENT EXPRESSION OF " &
--                      "AN ENCLOSING ARRAY AGGREGATE");

         Case_E1 :
         declare

            type Te2 is array (1 .. 2) of String (Ident_Int (3) .. 5);

            E1 : Te2;

         begin

            E1 := (1 .. 2 => "ABC");
            if (E1'First /= 1 or E1'Last /= 2)
              or else
              (E1 (1)'First /= 3 or E1 (1)'Last /= 5 or E1 (2)'First /= 3 or
               E1 (2)'Last /= 5)
            then
               Failed ("CASE 1 : INCORRECT BOUNDS");
            elsif E1 /= (1 .. 2 => "ABC") then
               Failed
                 ("CASE 1 : ARRAY DOES NOT " & "CONTAIN THE CORRECT VALUES");
            end if;

         end Case_E1;

--             COMMENT ("CASE E2 : ARRAY COMPONENT EXPRESSION OF " &
--                      "AN ENCLOSING RECORD AGGREGATE");

         Case_E2 :
         declare

            type Ter is record
               Rec : String (3 .. 5);
            end record;

            E2 : Ter;

         begin

            E2 := (Rec => "ABC");
            if E2.Rec'First /= 3 or E2.Rec'Last /= 5 then
               Failed ("CASE 2 : INCORRECT BOUNDS");
            elsif E2.Rec /= "ABC" then
               Failed ("CASE 2 : ARRAY DOES NOT " & "CONTAIN CORRECT VALUES");
            end if;

         end Case_E2;

--             COMMENT ("CASE E3 : NULL LITERAL OF AN ENCLOSING " &
--                      "ARRAY AGGREGATE");

         Case_E3 :
         declare

            type Te2 is array (1 .. 2) of String (3 .. Ident_Int (2));

            E3 : Te2;

         begin

            E3 := (1 .. 2 => "");
            if (E3'First /= 1 or E3'Last /= 2)
              or else
              (E3 (1)'First /= 3 or E3 (1)'Last /= 2 or E3 (2)'First /= 3 or
               E3 (2)'Last /= 2)
            then
               Failed ("CASE 3 : INCORRECT BOUND");
            elsif E3 /= (1 .. 2 => "") then
               Failed
                 ("CASE 3 : ARRAY DOES NOT CONTAIN " & "THE CORRECT VALUES");
            end if;

         end Case_E3;

--             COMMENT ("CASE E4 : ARRAY COMPONENT EXPRESSION OF "  &
--                      "AN ENCLOSING RECORD AGGREGATE THAT HAS A " &
--                      "DISCRIMINANT AND THE DISCRIMINANT DETER"   &
--                      "MINES THE BOUNDS OF THE COMPONENT");

         Case_E4 :
         declare

            subtype Ten is Integer range 1 .. 10;
            type Ter (A : Ten) is record
               Rec : String (3 .. A);
            end record;

            E4 : Ter (5);

         begin

            E4 := (Rec => "ABC", A => 5);
            if E4.Rec'First /= 3 or E4.Rec'Last /= 5 then
               Failed ("CASE 4 : INCORRECT BOUNDS");
            elsif E4.Rec /= "ABC" then
               Failed ("CASE 4 : ARRAY DOES NOT CONTAIN " & "CORRECT VALUES");
            end if;

         end Case_E4;

      end Case_E;

   end;

   Result;

end C43214f;
