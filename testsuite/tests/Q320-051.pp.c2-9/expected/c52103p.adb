-- C52103P.ADA

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

--    DIVISION  D : NULL LENGTHS NOT DETERMINABLE STATICALLY.

-- RM 07/20/81
-- SPS 3/22/83

with Report;
procedure C52103p is

   use Report;

begin

   Test
     ("C52103P",
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
   --                                FLOATING SELECTIONS ARE 10-3-12-
   --                                -5-14 ; THUS THE 8 SELECTIONS ARE
   --                                10-3-12-5-14-7-8-9 (IN THIS ORDER
   --                                ).)
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
   --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)
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

   --   (10) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
   --        DEFINED USING THE "BOX" COMPOUND SYMBOL.
   --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)

   declare

      type Tabox0 is array (Integer range <>, Integer range <>) of Integer;

      subtype Tabox01 is
        Tabox0
          (Ident_Int (1) .. Ident_Int (0),
           Ident_Int (0) .. Ident_Int (7));
      subtype Tabox02 is Tabox0;

      Arrx01 : Tabox01;
      Arrx02 : Tabox02
        (Ident_Int (7) .. Ident_Int (6),
         Ident_Int (20) .. Ident_Int (27));

   begin

      -- ARRAY ASSIGNMENT:

      Arrx02 := Arrx01;

   exception

      when others =>
         Failed ("EXCEPTION RAISED  -  SUBTEST 10");

   end;

   -------------------------------------------------------------------

   --    (3) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)

   declare

      type Ta3 is
        array (Integer range Ident_Int (100) .. Ident_Int (99)) of Integer;

      subtype Ta31 is Ta3;
      subtype Ta32 is Ta3;

      Arr31 : Ta31;
      Arr32 : Ta32;

   begin

      -- ARRAY ASSIGNMENT:

      Arr32 := Arr31;

   exception

      when others =>
         Failed ("EXCEPTION RAISED  -  SUBTEST 3");

   end;

   -------------------------------------------------------------------

   --   (12) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)

   declare

      type Tabox5 is array (Integer range <>) of Boolean;

      subtype Tabox51 is Tabox5 (Ident_Int (1) .. Ident_Int (5));

      Arrx51 : Tabox51;
      Arrx52 : Tabox5 (Ident_Int (5) .. Ident_Int (9));

   begin

      -- INITIALIZATION OF RHS ARRAY:

      for I in Ident_Int (1) .. Ident_Int (5) loop
         Arrx51 (I) := False; -- VALUES WILL BE:  F T F F T
      end loop;

      Arrx51 (2) := True;

      Arrx51 (5) := True;            -- RHS VALUES ARE:  F T F F T

      -- INITIALIZATION OF LHS ARRAY:

      for I in Ident_Int (5) .. Ident_Int (9) loop
         Arrx52 (I) := True; -- VALUES WILL BE:  T F T T F
      end loop;

      Arrx52 (6) := False;

      Arrx52 (9) := False;           -- LHS VALUES ARE:  T F T T F

      -- NULL SLICE ASSIGNMENT:

      Arrx52 (Ident_Int (6) .. Ident_Int (5)) :=
        Arrx51 (Ident_Int (4) .. Ident_Int (3));

      -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

      if Arrx52 (5) /= True or
        Arrx52 (6) /= False or
        Arrx52 (7) /= True or
        Arrx52 (8) /= True or
        Arrx52 (9) /= False
      then
         Failed ("SLICE ASSIGNMENT NOT CORRECT (VALUES)");
      end if;

   exception

      when others =>
         Failed ("EXCEPTION RAISED  -  SUBTEST 12");

   end;

   -------------------------------------------------------------------

   Result;

end C52103p;
