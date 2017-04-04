-- C52103L.ADA

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
--    MORE SPECIFICALLY, TEST THAT ARRAY ASSIGNMENTS WITH MATCHING
--    LENGTHS DO NOT CAUSE  CONSTRAINT_ERROR  TO BE RAISED AND
--    ARE PERFORMED CORRECTLY.
--    (OVERLAPS BETWEEN THE OPERANDS OF THE ASSIGNMENT STATEMENT
--    ARE TREATED ELSEWHERE.)

-- THIS IS THE SECOND FILE IN
--    DIVISION  C :  NON-NULL ARRAYS WHOSE LENGTHS ARE NOT DETERMINABLE
--                   STATICALLY.

-- RM 07/20/81
-- SPS 3/22/83

with Report;
procedure C52103l is

   use Report;

begin

   Test
     ("C52103L",
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

      type Tabox3 is array (Integer range <>) of Character;

      Arrx31 : Tabox3 (Ident_Int (11) .. Ident_Int (15));

   begin

      -- ARRAY ASSIGNMENT (WITH STRING AGGREGATE):

      Arrx31 := "QUINC"; -- "QUINC"(1..5)  SLIDES TO 11..15

      -- CHECKING THE VALUES AFTER THE ASSIGNMENT:

      if Arrx31 /= "QUINC" or
        Arrx31 (Ident_Int (11) .. Ident_Int (15)) /= "QUINC"
      then
         Failed ("ARRAY ASSIGNMENT NOT CORRECT (13)");
      end if;

   exception

      when others =>
         Failed ("EXCEPTION RAISED  -  SUBTEST 13");

   end;

   -------------------------------------------------------------------

   --    (6) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
   --

   declare

      type Ta61 is
        array (Integer range Ident_Int (11) .. Ident_Int (15)) of Character;

      Arr61 : Ta61;

   begin

      -- INITIALIZATION OF UNUSED COMPONENT OF LHS ARRAY:

      Arr61 (Ident_Int (11) .. Ident_Int (11)) := "Q";

      -- SLICE ASSIGNMENT:

      Arr61 (Ident_Int (12) .. Ident_Int (15)) := "UINC";
      -- "UINC"(1..4) SLIDES TO 12..15

      -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

      if Arr61 /= "QUINC" or
        Arr61 (Ident_Int (11) .. Ident_Int (15)) /= "QUINC"
      then
         Failed ("SLICE ASSIGNMENT NOT CORRECT (6)");
      end if;

   exception

      when others =>
         Failed ("EXCEPTION RAISED  -  SUBTEST 6");

   end;

   -------------------------------------------------------------------

   Result;

end C52103l;
