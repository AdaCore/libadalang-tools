-- C52103A.ADA

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

--    DIVISION  A : STATICALLY-DETERMINABLE NON-NULL LENGTHS.

-- RM 07/20/81
-- SPS 2/18/83

with Report;
procedure C52103a is

   use Report;

begin

   Test
     ("C52103A",
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
   --                              ( THE SELECTIONS ARE  7 , 8 , 9 ,
   --                                AND PRECISELY 5 CASES FROM THE
   --                                TWO 5-CASE SERIES 2-3-4-5-6 AND
   --                                                  10-11-12-13-14)
   --
   --                              ( IN THE CURRENT DIVISION, THE 5
   --                                FLOATING SELECTIONS ARE 2-11-4-
   --                                -13-6 ; THUS THE 8 SELECTIONS ARE
   --                                2-11-4-13-6-7-8-9 (IN THIS ORDER)
   --                                .)
   --
   --
   --                              ( EACH DIVISION COMPRISES 3 FILES,
   --                                COVERING RESPECTIVELY THE FIRST
   --                                3 , NEXT 2 , AND LAST 3 OF THE 8
   --                                SELECTIONS FOR THE DIVISION.)
   --
   --
   --    (1) ARRAY OBJECTS DECLARED IN THE SAME DECLARATION.
   --        (TWO-DIMENSIONAL; NON-CONSTRAINABLE TYPEMARK.)
   --
   --        (THIS WILL BE THE ONLY CASE INVOLVING OBJECTS DECLARED
   --        IN THE SAME DECLARATION.)
   --
   --
   --    (2) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
   --        DEFINED WITHOUT EVER USING THE "BOX" COMPOUND SYMBOL.
   --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)
   --
   --        (SLICING IS ILLEGAL; SINCE IN THIS TEST WE ARE NEVER
   --        USING AGGREGATES
   --                (EXCEPT FOR ONE-DIMENSIONAL ARRAYS OF CHARACTERS;
   --                SEE  (5) )
   --        AND WE ARE NOT USING CONVERSION-TO-CONSTRAINED-TYPEMARKS
   --                (AS IN    T1(ARR)   , WHERE  ARR  IS AN ARRAY
   --                OBJECT AND  T1  IS AN ARRAY TYPEMARK SIMILAR
   --                -- AS MORE PRECISELY SPECIFIED IN  RM 4.6(B) --
   --                TO THE TYPEMARK OF  ARR ),
   --        THE ARRAY ASSIGNMENT CANNOT INVOLVE ANY SLIDING,
   --        AND THE TYPEMARKS ARE ESSENTIALLY THE SAME.)
   --
   --
   --    (3) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)
   --
   --        (SINCE WE ARE NOT USING AGGREGATES
   --        AND WE ARE NOT USING CONVERSION-TO-CONSTRAINED-TYPEMARKS,
   --        THE ARRAY ASSIGNMENT CANNOT INVOLVE ANY SLIDING,
   --        AND THE TYPEMARKS ARE ESSENTIALLY THE SAME.)
   --
   --
   --    (4) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)
   --
   --        (THE ASSIGNMENT MAY REQUIRE SLIDING.)
   --
   --        (MOST SUBSEQUENT SUBCASES IN THIS TEST (OTHER THAN NULL
   --        ASSIGNMENTS) WILL INVOLVE SLIDING; WE ASSUME THAT
   --        SUBCASES WHICH WORK IN CONJUNCTION WITH SLIDING  WORK
   --        ALSO WHEN NO SLIDING IS INVOLVED.)
   --
   --
   --    (5) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
   --
   --        (STRING LITERALS ARE THE ONLY AGGREGATES WE ARE USING
   --        IN THIS TEST.  TO FORCE SLIDING, THE LOWER LIMIT IMPLIED
   --        BY THE TYPEMARK WILL NOT BE  1 .)
   --
   --
   --    (6) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
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
   --    (-) SPECIAL CASES:  NULL ARRAYS....... TREATED IN DIVISION B.
   --                        SUPERLONG ARRAYS.. (TREATED FOR DYNAMIC
   --                                            ARRAYS ONLY,
   --                                            DIVISIONS C AND D .)
   --
   --
   --    (-) THE DYNAMIC-ARRAY COUNTERPARTS OF THESE TESTS ARE IN DI-
   --        VISIONS C (FOR NON-NULL ARRAYS) AND D (FOR NULL ARRAYS).
   --
   --

   -------------------------------------------------------------------

   --    (2) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
   --        DEFINED WITHOUT EVER USING THE "BOX" COMPOUND SYMBOL.
   --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)

   declare

      type Ta21 is
        array (Integer range 1 .. 5, Integer range 0 .. 7) of Integer;

      subtype Ta22 is Ta21;

      Arr21 : Ta21;
      Arr22 : Ta22;

   begin

      -- INITIALIZATION OF RHS ARRAY:

      for I in 1 .. 5 loop

         for J in 0 .. 7 loop
            Arr21 (I, J) := I * I * J;
         end loop;

      end loop;

      -- ARRAY ASSIGNMENT:

      Arr22 := Arr21;

      -- CHECKING THE VALUES AFTER THE ARRAY ASSIGNMENT:

      for I in 1 .. 5 loop

         for J in 0 .. 7 loop

            if Arr22 (I, J) /= (I - 0) * (I - 0) * (J - 0) then
               Failed ("ARRAY ASSIGNMENT NOT CORRECT");
            end if;

         end loop;

      end loop;

   exception

      when others =>
         Failed ("EXCEPTION RAISED  -  SUBTEST 2");

   end;

   -------------------------------------------------------------------

   --   (11) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)

   declare

      type Tabox1 is array (Integer range <>) of Integer;

      subtype Tabox11 is Tabox1 (1 .. 5);

      Arrx11 : Tabox11;
      Arrx12 : Tabox1 (5 .. 9);

   begin

      -- INITIALIZATION OF RHS ARRAY:

      for I in 1 .. 5 loop
         Arrx11 (I) := I * I;
      end loop;

      -- ARRAY ASSIGNMENT:

      Arrx12 := Arrx11;

      -- CHECKING THE VALUES AFTER THE ARRAY ASSIGNMENT:

      for I in 5 .. 9 loop

         if Arrx12 (I) /= (I - 4) * (I - 4) then
            Failed ("ARRAY ASSIGNMENT NOT CORRECT (11)");
         end if;

      end loop;

   exception

      when others =>
         Failed ("EXCEPTION RAISED  -  SUBTEST 11");

   end;

   -------------------------------------------------------------------

   --    (4) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)

   declare

      type Ta42 is array (Integer range 1 .. 5) of Boolean;

      subtype Ta41 is Ta42;

      Arr41 : Ta41;
      Arr42 : Ta42;

   begin

      -- INITIALIZATION OF RHS ARRAY:

      for I in 1 .. 5 loop
         Arr41 (I) := False; -- VALUES WILL BE:  F T F F T
      end loop;

      Arr41 (2) := True;

      Arr41 (5) := True;            -- RHS VALUES ARE:  F T F F T

      -- INITIALIZATION OF UNUSED COMPONENT OF LHS ARRAY:

      Arr42 (1) := True;

      -- SLICE ASSIGNMENT:

      Arr42 (2 .. 5) := Arr41 (1 .. 4);

      -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

      for I in 2 .. 5 loop

         if Arr42 (I) /= False and I /= 3 then
            Failed ("SLICE ASSIGNMENT NOT CORRECT (VALUES)");
         elsif Arr42 (I) /= True and I = 3 then
            Failed ("SLICE ASSIGNMENT NOT CORRECT (VALUES)");
         end if;

      end loop;

      if Arr42 (1) /= True then
         Failed ("SLICE ASSIGNMENT NOT CORRECT (SLIDING)");
      end if;

   exception

      when others =>
         Failed ("EXCEPTION RAISED  -  SUBTEST 4");

   end;

   -------------------------------------------------------------------

   Result;

end C52103a;
