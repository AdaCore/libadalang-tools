-- C52104G.ADA

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

-- THIS IS THE SECOND FILE IN
--    DIVISION  B : STATICALLY-DETERMINABLE NULL LENGTHS.

-- RM 07/20/81
-- SPS 3/22/83
-- JBG 4/24/84

with Report;
procedure C52104g is

   use Report;

begin

   Test
     ("C52104G",
      "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
      " ASSIGNMENTS  THE LENGTHS MUST MATCH");

   --                              ( EACH DIVISION COMPRISES 3 FILES,
   --                                COVERING RESPECTIVELY THE FIRST
   --                                3 , NEXT 2 , AND LAST 3 OF THE 8
   --                                SELECTIONS FOR THE DIVISION.)

   -------------------------------------------------------------------

   --   (13) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .

   declare

      type Tabox3 is array (Natural range <>) of Character;

      Arrx31 : Tabox3 (11 .. 10) := "";

   begin

      -- ARRAY ASSIGNMENT (WITH STRING AGGREGATE):

      Arrx31 := "AZ";
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 13");

   exception

      when Constraint_Error =>

         -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

         if Arrx31 /= "" then
            Failed ("ARRAY ASSIGNMENT NOT CORRECT (13)");
         end if;

      when others =>

         Failed ("WRONG EXCEPTION RAISED  -  SUBTEST 13");

   end;

   -------------------------------------------------------------------

   --   (14) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .

   declare

      type Tabox4 is array (Integer range <>) of Character;

      subtype Tabox42 is Tabox4 (11 .. 15);

      Arrx42 : Tabox42;

   begin

      -- INITIALIZATION OF LHS ARRAY:

      Arrx42 := "QUINC";

      -- NULL SLICE ASSIGNMENT:

      Arrx42 (13 .. 12) := "ABCD";
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 14");

   exception

      when Constraint_Error =>

         -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

         if Arrx42 /= "QUINC" or Arrx42 (11 .. 15) /= "QUINC" then
            Failed ("LHS ARRAY ALTERED  (14)");
         end if;

      when others =>

         Failed ("WRONG EXCEPTION RAISED  -  SUBTEST 14");

   end;

   -------------------------------------------------------------------

   Result;

end C52104g;
