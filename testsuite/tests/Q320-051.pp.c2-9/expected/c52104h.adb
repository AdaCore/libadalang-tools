-- C52104H.ADA

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
-- CHECK THAT LENGTHS MUST MATCH IN ARRAY AND SLICE ASSIGNMENTS.
--    MORE SPECIFICALLY, TEST THAT ATTEMPTED ASSIGNMENTS BETWEEN
--    ARRAYS WITH NON-MATCHING LENGTHS LEAVE THE DESTINATION ARRAY
--    INTACT AND CAUSE  CONSTRAINT_ERROR  TO BE RAISED.
--    (OVERLAPS BETWEEN THE OPERANDS OF THE ASSIGNMENT STATEMENT
--    ARE TREATED ELSEWHERE.)

-- THIS IS THE THIRD FILE IN
--    DIVISION  B : STATICALLY-DETERMINABLE NULL LENGTHS.

-- RM 07/20/81
-- SPS 3/22/83

with Report;
procedure C52104h is

   use Report;

begin

   Test
     ("C52104H",
      "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
      " ASSIGNMENTS  THE LENGTHS MUST MATCH");

   --                              ( EACH DIVISION COMPRISES 3 FILES,
   --                                COVERING RESPECTIVELY THE FIRST
   --                                3 , NEXT 2 , AND LAST 3 OF THE 8
   --                                SELECTIONS FOR THE DIVISION.)

   -------------------------------------------------------------------

   --    (7) UNSLICED OBJECTS OF THE PREDEFINED TYPE  'STRING'  (BY
   --        THEMSELVES).

   declare

      Arr71 : String (1 .. 1) := "A";
      Arr72 : String (5 .. 4) := "";

   begin

      -- STRING ASSIGNMENT:

      Arr72 := Arr71;
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 7");

   exception

      when Constraint_Error =>

         -- CHECKING THE VALUES AFTER THE STRING ASSIGNMENT:

         if Arr72 /= "" then
            Failed ("ORIGINAL VALUE ALTERED (7)");
         end if;

      when others =>
         Failed ("WRONG EXCEPTION RAISED  -  SUBTEST 7");

   end;

   -------------------------------------------------------------------

   --    (8) SLICED OBJECTS OF THE PREDEFINED TYPE  'STRING' , WITH
   --        STRING LITERALS.
   --

   declare

      Arr82 : String (5 .. 9);

   begin

      -- INITIALIZATION OF LHS ARRAY:

      Arr82 (5 .. 9) := "QUINC";

      -- STRING LITERAL ASSIGNMENT:

      Arr82 (5 .. 9) (6 .. 9) (6 .. 5) := "ABC";
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 8");

   exception

      when Constraint_Error =>

         -- CHECKING THE VALUES AFTER THE STRING ASSIGNMENT:

         if Arr82 /= "QUINC" or Arr82 (5 .. 9) /= "QUINC" then
            Failed ("ORIGINAL VALUE ALTERED (8)");
         end if;

      when others =>

         Failed ("WRONG EXCEPTION RAISED  -  SUBTEST 8");

   end;

   -------------------------------------------------------------------

   --    (9) SLICED OBJECTS OF THE PREDEFINED TYPE  'STRING'  (BY
   --        THEMSELVES).
   --

   declare

      subtype Ta92 is String (5 .. 9);

      Arr91 : String (1 .. 5) := "ABCDE";
      Arr92 : Ta92;

   begin

      -- INITIALIZATION OF LHS ARRAY:

      Arr92 (5 .. 9) := "QUINC";

      -- STRING SLICE ASSIGNMENT:

      Arr92 (5 .. 9) (6 .. 9) (8 .. 7) := Arr91 (1 .. 5) (5 .. 7);
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 9");

   exception

      when Constraint_Error =>

         -- CHECKING THE VALUES AFTER THE STRING ASSIGNMENT:

         if Arr92 /= "QUINC" or Arr92 (5 .. 9) /= "QUINC" then
            Failed ("ORIGINAL VALUE ALTERED (9)");
         end if;

      when others =>

         Failed ("WRONG EXCEPTION RAISED  -  SUBTEST 9");

   end;

   -------------------------------------------------------------------

   Result;

end C52104h;
