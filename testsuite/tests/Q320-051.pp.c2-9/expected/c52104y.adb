-- C52104Y.ADA

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

-- THIS IS A SPECIAL CASE IN

--    DIVISION  D :  NULL ARRAYS WHOSE LENGTHS ARE NOT DETERMINABLE
--                   STATICALLY

-- WHICH (THE SPECIAL CASE) TREATS TWO-DIMENSIONAL ARRAYS WHOSE LENGTH
--    ALONG ONE DIMENSION IS GREATER THAN  INTEGER'LAST  AND WHOSE
--    LENGTH ALONG THE OTHER DIMENSION IS  0 .
--    AN ADDITIONAL OBJECTIVE OF THIS TEST IS TO CHECK WHETHER LENGTH
--    COMPARISONS (AND LENGTH COMPUTATIONS) CAUSE  CONSTRAINT_ERROR
--    TO BE RAISED.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- RM  07/31/81
-- SPS 03/22/83
-- JBG 06/16/83
-- EG 10/28/85 FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387.
-- MRM 03/30/93 REMOVE NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report;
procedure C52104y is

   use Report;

begin

   Test
     ("C52104Y",
      "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
      " ASSIGNMENTS, THE LENGTHS MUST MATCH");

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

   -------------------------------------------------------------------

   --   (10) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
   --        DEFINED USING THE "BOX" COMPOUND SYMBOL.
   --        (TWO-DIMENSIONAL ARRAYS OF BOOLEANS.)

   Constr_Err : begin          -- THIS BLOCK CATCHES CONSTRAINT_ERROR IF IT IS
      -- RAISED BY THE SUBTYPE DECLARATION.

      Dcl_Arr : declare

         type Tabox5 is array (Integer range <>, Integer range <>) of Boolean;
         pragma Pack (Tabox5);

         subtype Tabox52 is
           Tabox5
             (Ident_Int (13) .. Ident_Int (13),
              Ident_Int (-6) .. Ident_Int (Integer'Last - 4));

      begin

         Comment
           ("NO CONSTRAINT_ERROR FOR NON-NULL ARRAY SUBTYPE " &
            "WHEN ONE DIMENSION HAS INTEGER'LAST + 3 " &
            "COMPONENTS");

         Obj_Dcl : declare   -- THIS BLOCK DECLARES ONE NULL ARRAY AND ONE
            -- PACKED BOOLEAN ARRAY WITH INTEGER'LAST + 3 COMPONENTS; STORAGE
            -- ERROR MAY BE RAISED.

            Arrx51 : Tabox5
              (Ident_Int (13) .. Ident_Int (12),
               Ident_Int (-6) .. Ident_Int (Integer'Last - 4));
            Arrx52 : Tabox52;     -- BIG ARRAY HERE.

         begin

            Comment
              ("NO CONSTRAINT OR STORAGE ERROR WHEN ARRAY " &
               "WITH INTEGER'LAST+3 COMPONENTS ALLOCATED");

            -- NULL ARRAY ASSIGNMENT:

            Arrx52 := Arrx51;
            Failed ("EXCEPTION NOT RAISED  (10)");

         exception

            when Constraint_Error =>
               Comment
                 ("CONSTRAINT_ERROR RAISED WHEN " &
                  "CHECKING LENGTHS FOR ARRAY HAVING " &
                  "> INTEGER'LAST COMPONENTS ON ONE " &
                  "DIMENSION");

            when others =>
               Failed ("OTHER EXCEPTION RAISED - SUBTEST 10");

         end Obj_Dcl;

      exception

         when Storage_Error =>
            Comment
              ("STORAGE_ERROR RAISED WHEN DECLARING ONE " &
               "PACKED BOOLEAN ARRAY WITH INTEGER'LAST " &
               "+ 3 COMPONENTS");
         when Constraint_Error =>
            Comment
              ("CONSTRAINT_ERROR RAISED WHEN DECLARING " &
               "ONE PACKED BOOLEAN ARRAY WITH " &
               "INTEGER'LAST + 3 COMPONENTS");
         when others =>
            Failed ("SOME EXCEPTION RAISED - 3");

      end Dcl_Arr;

   exception

      when Constraint_Error =>
         Comment
           ("CONSTRAINT_ERROR RAISED WHEN DECLARING AN " &
            "ARRAY SUBTYPE WITH INTEGER'LAST + 3 " &
            "COMPONENTS");

      when Storage_Error =>
         Failed ("STORAGE_ERROR RAISED FOR TYPE DECLARATION");

      when others =>
         Failed ("OTHER EXCEPTION RAISED - 4");

   end Constr_Err;

   Result;

end C52104y;
