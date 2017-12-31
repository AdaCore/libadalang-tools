-- C52104K.ADA

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

--    DIVISION  C :  NON-NULL LENGTHS NOT DETERMINABLE STATICALLY.

-- RM 07/20/81
-- SPS 3/22/83

with Report;
procedure C52104k is

   use Report;

begin

   Test
     ("C52104K",
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

   -- CASES DISTINGUISHED: ( 8 SELECTED CASES ARE IMPLEMENTED)
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
   --    (-) SPECIAL CASES:  NULL ARRAYS....... TREATED IN DIVISION B.
   --                        SUPERLONG ARRAYS.. (TREATED FOR DYNAMIC
   --                                            ARRAYS ONLY,
   --                                            DIVISIONS C AND D .)
   --
   --
   --    (-) THE STATIC-ARRAY COUNTERPARTS OF THESE TESTS ARE IN DI-
   --        VISIONS A (FOR NON-NULL ARRAYS) AND B (FOR NULL ARRAYS).
   --
   --

   -------------------------------------------------------------------

   --    (1..6: NOT APPLICABLE)
   --
   --

   -------------------------------------------------------------------

   --   (10) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
   --        DEFINED USING THE "BOX" COMPOUND SYMBOL.
   --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)

   declare

      type Tabox0 is array (Integer range <>, Integer range <>) of Integer;

      subtype Tabox01 is
        Tabox0 (Ident_Int (1) .. Ident_Int (5),
           Ident_Int (0) .. Ident_Int (7));
      subtype Tabox02 is
        Tabox0 (Ident_Int (0) .. Ident_Int (5),
           Ident_Int (2) .. Ident_Int (9));

      Arrx01 : Tabox01;
      Arrx02 : Tabox02;

   begin

      -- INITIALIZATION OF RHS ARRAY:

      for I in Ident_Int (1) .. Ident_Int (5) loop

         for J in Ident_Int (0) .. Ident_Int (7) loop
            Arrx01 (I, J) := I * I * J;
         end loop;

      end loop;

      -- INITIALIZATION OF LHS ARRAY:

      for I in Ident_Int (0) .. Ident_Int (5) loop

         for J in Ident_Int (2) .. Ident_Int (9) loop
            Arrx02 (I, J) := I * I * J * 3;
         end loop;

      end loop;

      -- ARRAY ASSIGNMENT:

      Arrx02 := Arrx01;
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 10");

   exception

      when Constraint_Error =>

         -- CHECKING THE VALUES AFTER THE ARRAY ASSIGNMENT:

         for I in Ident_Int (0) .. Ident_Int (5) loop

            for J in Ident_Int (2) .. Ident_Int (9) loop

               if Arrx02 (I, J) /= I * I * J * 3 then
                  Failed ("ORIG. VALUE ALTERED (10)");
               end if;

            end loop;

         end loop;

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

      subtype Tabox11 is Tabox1 (Ident_Int (1) .. Ident_Int (5));

      Arrx11 : Tabox11;
      Arrx12 : Tabox1 (Ident_Int (6) .. Ident_Int (9));

   begin

      -- INITIALIZATION OF RHS ARRAY:

      for I in Ident_Int (1) .. Ident_Int (5) loop

         Arrx11 (I) := I * I;

      end loop;

      -- INITIALIZATION OF LHS ARRAY:

      for I in Ident_Int (6) .. Ident_Int (9) loop
         Arrx12 (I) := I * I * 3;
      end loop;

      -- ARRAY ASSIGNMENT:

      Arrx12 := Arrx11;
      Failed ("EXCEPTION NOT RAISED  -  SUBTEST 11");

   exception

      when Constraint_Error =>

         -- CHECKING THE VALUES AFTER THE ARRAY ASSIGNMENT:

         for I in Ident_Int (6) .. Ident_Int (9) loop

            if Arrx12 (I) /= I * I * 3 then
               Failed ("ORIG. VALUE ALTERED (11)");
            end if;

         end loop;

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

      subtype Tabox51 is Tabox5 (Ident_Int (1) .. Ident_Int (5));

      Arrx51 : Tabox51;
      Arrx52 : Tabox5 (Ident_Int (5) .. Ident_Int (9));

   begin

      -- INITIALIZATION OF LHS ARRAY:

      for I in Ident_Int (5) .. Ident_Int (9) loop
         Arrx52 (I) := False;
      end loop;

      -- INITIALIZATION OF RHS ARRAY:

      for I in Ident_Int (1) .. Ident_Int (5) loop
         Arrx51 (I) := True;
      end loop;

      -- SLICE ASSIGNMENT:

      Arrx52 (Ident_Int (6) .. Ident_Int (9)) :=
        Arrx51 (Ident_Int (3) .. Ident_Int (3));
      Failed ("EXCEPTION NOT RAISED  (12)");

   exception

      when Constraint_Error =>

         -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

         for I in Ident_Int (5) .. Ident_Int (9) loop

            if Arrx52 (I) /= False then
               Failed ("LHS ARRAY ALTERED  ( 12 ) ");
            end if;

         end loop;

      when others =>
         Failed ("EXCEPTION RAISED  -  SUBTEST 12");

   end;

   -------------------------------------------------------------------

   Result;

end C52104k;
