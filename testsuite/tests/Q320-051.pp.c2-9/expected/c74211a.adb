-- C74211A.ADA

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
-- CHECK THAT WITHIN THE PACKAGE SPECIFICATION AND BODY, ANY EXPLICIT
-- DECLARATIONS OF OPERATORS AND SUBPROGRAMS HIDE ANY OPERATIONS WHICH
-- ARE IMPLICITLY DECLARED AT THE POINT OF THE FULL DECLARATION,
-- REGARDLESS OF THE ORDER OF OCCURENCE OF THE DECLARATIONS.

-- CHECK THAT IMPLICITLY DECLARED DERIVED SUBPROGRAMS HIDE IMPLICITLY
-- DECLARED PREDEFINED OPERATORS, REGARDLESS OF THE ORDER OF OCCURENCE
-- OF THE DECLARATIONS.

-- DSJ 4/28/83
-- JBG 9/23/83

--   A) EXPLICIT DECLARATION HIDES LATER IMPLICIT DECL OF PREDEFINED OP.
--   B)     "        "         "   LATER     "      "  "  DERIVED OP.
--   C)     "        "         "   EARLIER   "      "  "  PREDEFINED OP.
--   D)     "        "         "   EARLIER   "      "  "  DERIVED OP.

with Report;
procedure C74211a is

   use Report;

begin

   Test
     ("C74211A",
      "CHECK THAT HIDING OF IMPLICITLY DECLARED " &
      "OPERATORS AND DERIVED SUBPROGRAMS IS DONE " &
      "CORRECTLY REGARDLESS OF ORDER OF DECL'S");

   declare

      package P1 is
         type T1 is range 1 .. 50;
         C1 : constant T1 := T1 (Ident_Int (2));
         D1 : constant T1 := C1 + C1;        -- PREDEFINED "+"
         function "+" (L, R : T1) return T1; -- C) FOR "+".
         function "-" (L, R : T1) return T1; -- C) FOR "-".
         function "/" (L, R : T1) return T1;
      end P1;

      use P1;

      package body P1 is
         A, B : T1 := 3;

         function "+" (L, R : T1) return T1 is
         begin
            if L = R then
               return 1;
            else
               return 2;
            end if;
         end "+";

         function "-" (L, R : T1) return T1 is
         begin
            if L = R then
               return 3;
            else
               return 4;
            end if;
         end "-";

         function "/" (L, R : T1) return T1 is
         begin
            if L = R then
               return T1 (Ident_Int (Integer (L)));
            else
               return T1 (Ident_Int (50));
            end if;
         end "/";

      begin
         if D1 /= 4 then
            Failed ("WRONG PREDEFINED OPERATION - '+' ");
         end if;

         if D1 + C1 /= 2 then
            Failed ("IMPLICIT '+' NOT HIDDEN BY EXPLICIT '+'");
         end if;

         if A + B /= 1 then
            Failed
              ("IMPLICIT DECLARATION NOT HIDDEN " &
               "BY EXPLICIT DECLARATION - '+' ");
         end if;

         if A - B /= 3 then
            Failed
              ("IMPLICIT DECLARATION NOT HIDDEN " &
               "BY EXPLICIT DECLARATION - '-' ");
         end if;

         if A * B /= 9 then
            Failed ("WRONG PREDEFINED OPERATION - '*' ");
         end if;

         if B / A /= T1 (Ident_Int (3)) then
            Failed ("NOT REDEFINED '/' ");
         end if;
      end P1;

      package P2 is
         type T2 is private;
         X, Y : constant T2;
         function "+" (L, R : T2) return T2;     -- B)
         function "*" (L, R : T2) return T2;     -- A)
      private
         type T2 is new T1;                 -- B) +; A) *
         Z : T2 := T2 (Ident_Int (3)) / 4;      -- Z = 50 USING
         -- DERIVED /
         function "/" (L, R : T2) return T2;  -- D) FOR /
         X, Y : constant T2 := 3;
      end P2;

      package body P2 is
         function "+" (L, R : T2) return T2 is
         begin
            if L = R then
               return T2 (Ident_Int (5));
            else
               return T2 (Ident_Int (6));
            end if;
         end "+";

         function "*" (L, R : T2) return T2 is
         begin
            if L = R then
               return T2 (Ident_Int (7));
            else
               return T2 (Ident_Int (8));
            end if;
         end "*";

         function "/" (L, R : T2) return T2 is
         begin
            if L = R then
               return T2 (Ident_Int (9));
            else
               return T2 (Ident_Int (10));
            end if;
         end "/";
      begin
         if X + Y /= 5 then
            Failed
              ("DERIVED SUBPROGRAM NOT HIDDEN BY " &
               "EXPLICIT DECLARATION - '+' ");
         end if;

         if Y - X /= 3 then
            Failed
              ("PREDEFINED OPERATOR NOT HIDDEN BY " &
               "DERIVED SUBPROGRAM - '-' ");
         end if;

         if X * Y /= 7 then
            Failed
              ("PREDEFINED OPERATOR NOT HIDDEN BY " &
               "EXPLICIT DECLARATION - '*' ");
         end if;

         if Y / X /= T2 (Ident_Int (9)) then
            Failed
              ("DERIVED OPERATOR NOT HIDDEN BY " &
               "EXPLICIT DECLARATION - '/' ");
         end if;

         if Z /= 50 then
            Failed
              ("DERIVED OPERATOR HIDDEN PREMATURELY " &
               " BY REDECLARED OPERATOR");
         end if;

      end P2;

   begin

      null;

   end;

   Result;

end C74211a;
