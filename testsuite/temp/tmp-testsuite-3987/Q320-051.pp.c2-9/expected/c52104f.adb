-- C52104F.ADA

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
--    ARE TREATED ELSWEWHERE.)

--    DIVISION  B : STATICALLY-DETERMINABLE NULL LENGTHS.

-- RM 07/20/81
-- SPS 10/27/82

with Report;
procedure C52104f is

   use Report;

begin

   Test
     ("C52104F",
      "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
      " ASSIGNMENTS  THE LENGTHS MUST MATCH");

   -- IN THIS TEST WE CAN'T USE AGGREGATE ASSIGNMENT (EXCEPT WHEN
   --    THE AGGREGATES ARE STRING LITERALS); THEREFORE:
   --
   --    (1) ARRAYS WILL BE INITIALIZED BY INDIVIDUAL ASSIGNMENTS;
   --    (2) CAN'T USE NON-NULL CONSTANT ARRAYS.

   -- WE ASSUME THAT IN AN ARRAY_TYPE_DEFINITION THE INDEX PORTION
   --    AND THE COMPONENT_TYPE PORTION ARE FUNCTIONALLY ORTHOGONAL
   --    ALSO AT THE IMPLEMENTATION LEVEL, I.E. THAT THE CORRECTNESS
   --    OF THE ACCESSING MECHANISM FOR ARRAYS DOES NOT DEPEND ON
   --    COMPONENT_TYPE.  ACCORDINGLY WE ARE TESTING FOR SOME BUT
   --    NOT ALL KINDS OF COMPONENT_TYPE.  (COMPONENT_TYPES INCLUDED:
   --    INTEGER , CHARACTER , BOOLEAN .)

   -- CASES DISTINGUISHED:         ( 8 SELECTED CASES ARE IMPLEMENTED)
   --
   --                              ( THE 8 SELECTIONS ARE THE 5-CASE
   --                                SERIES 10-11-12-13-14 FOLLOWED
   --                                BY  7 , 8 , 9 (IN THIS ORDER). )
   --
   --
   --                              ( EACH DIVISION COMPRISES 3 FILES,
   --                                COVERING RESPECTIVELY THE FIRST
   --                                3 , NEXT 2 , AND LAST 3 OF THE 8
   --                                SELECTIONS FOR THE DIVISION.)
   --
   --
   --    (1..6) (DO NOT APPLY TO NON-MATCHING OBJECTS, SINCE WE WANT
   --        THE OBJECTS TO HAVE THE   S A M E   BASE TYPE.)
   --
   --
   --    (7) UNSLICED OBJECTS OF THE PREDEFINED TYPE  'STRING'  (BY
   --        THEMSELVES).
   --
   --
   --    (8) SLICED OBJECTS OF THE PREDEFINED TYPE  'STRING' , WITH
   --        STRING LITERALS.
   --
   --
   --    (9) SLICED OBJECTS OF THE PREDEFINED TYPE  'STRING'  (BY
   --        THEMSELVES).
   --
   --
   --    (-) CONSTRAINABLE TYPES:  ONLY SUBTESTS   2,  3,  4,  5,  6
   --        WILL BE REPLICATED  --  AS SUBTESTS  10, 11, 12, 13, 14 .
   --
   --
   --   (10) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
   --        DEFINED USING THE "BOX" COMPOUND SYMBOL.
   --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)
   --
   --
   --   (11) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)
   --
   --
   --   (12) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)
   --
   --
   --   (13) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
   --
   --        (STRING LITERALS ARE THE ONLY AGGREGATES WE ARE USING
   --        IN THIS TEST.  TO FORCE SLIDING, THE LOWER LIMIT IMPLIED
   --        BY THE TYPEMARK WILL NOT BE  1 .)
   --
   --
   --   (14) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
   --
   --
   --
   --    (-) SPECIAL CASES:  SUPERLONG ARRAYS.. (TREATED FOR DYNAMIC
   --                                            ARRAYS ONLY,
   --                                            DIVISIONS C AND D .)
   --
   --
   --    (-) THE DYNAMIC-ARRAY COUNTERPARTS OF THESE TESTS ARE IN DI-
   --        VISIONS C (FOR NON-NULL ARRAYS) AND D (FOR NULL ARRAYS).
   --
   --

   -------------------------------------------------------------------

   --    (1 .. 6: NOT APPLICABLE)
   --
   --

   -------------------------------------------------------------------

   --   (10) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
   --        DEFINED USING THE "BOX" COMPOUND SYMBOL.
   --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)

   declare

      type Tabox0 is array (Integer range <>, Integer range <>) of Integer;

      subtype Tabox01 is Tabox0 (1 .. 1, 0 .. 7);
      subtype Tabox02 is Tabox0;

      Arrx01 : Tabox01;
      Arrx02 : Tabox02 (1 .. 0, 0 .. 7);

   begin

      -- ARRAY ASSIGNMENT:

      Arrx02 := Arrx01;
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 10");

   exception

      when Constraint_Error =>

         null;

      when others =>

         Failed ("WRONG EXCEPTION RAISED  -  SUBTEST 10");

   end;

   -------------------------------------------------------------------

   --   (11) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)

   declare

      type Tabox1 is array (Integer range <>) of Integer;

      subtype Tabox11 is Tabox1 (4 .. 5);

      Arrx11 : Tabox11;
      Arrx12 : Tabox1 (5 .. 4);

   begin

      -- ARRAY ASSIGNMENT:

      Arrx12 := Arrx11;
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 11");

   exception

      when Constraint_Error =>

         null;

      when others =>

         Failed ("WRONG EXCEPTION RAISED  -  SUBTEST 11");

   end;

   -------------------------------------------------------------------

   --   (12) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)

   declare

      type Tabox5 is array (Integer range <>) of Boolean;

      subtype Tabox51 is Tabox5 (1 .. 5);

      Arrx51 : Tabox51;
      Arrx52 : Tabox5 (5 .. 9);

   begin

      -- INITIALIZATION OF RHS ARRAY:

      for I in 1 .. 5 loop
         Arrx51 (I) := False; -- VALUES WILL BE:  F T F F T
      end loop;

      Arrx51 (2) := True;

      Arrx51 (5) := True;            -- RHS VALUES ARE:  F T F F T

      -- INITIALIZATION OF LHS ARRAY:

      for I in 5 .. 9 loop
         Arrx52 (I) := True; -- VALUES WILL BE:  T F T T F
      end loop;

      Arrx52 (6) := False;

      Arrx52 (9) := False;           -- LHS VALUES ARE:  T F T T F

      -- NULL SLICE ASSIGNMENT:

      Arrx52 (6 .. 5) := Arrx51 (4 .. 4);
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 12");

   exception

      when Constraint_Error =>

         -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:
         if Arrx52 (5) /= True or
           Arrx52 (6) /= False or
           Arrx52 (7) /= True or
           Arrx52 (8) /= True or
           Arrx52 (9) /= False
         then
            Failed ("LHS ARRAY ALTERED  (12)");
         end if;

      when others =>

         Failed ("WRONG EXCEPTION RAISED  -  SUBTEST 12");

   end;

   -------------------------------------------------------------------

   Result;

end C52104f;
