-- C52103X.ADA

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

-- THIS IS A SPECIAL CASE IN

--    DIVISION  C :  NON-NULL ARRAYS WHOSE LENGTHS ARE NOT DETERMINABLE
--                   STATICALLY

-- WHICH TREATS ARRAYS OF LENGTH GREATER THAN INTEGER'LAST .
--    AN ADDITIONAL OBJECTIVE OF THIS TEST IS TO CHECK WHETHER LENGTH
--    COMPARISONS (AND LENGTH COMPUTATIONS) CAUSE
--    CONSTRAINT_ERROR TO BE RAISED.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- RM  07/31/81
-- SPS 10/26/82
-- JBG 06/15/83
-- EG  11/02/84
-- EG 10/28/85 FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387.
-- JRK 06/24/86 FIXED COMMENTS ABOUT NUMERIC_ERROR/CONSTRAINT_ERROR. MRM
-- 03/30/93 REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report;
procedure C52103x is

   use Report;

begin

   Test
     ("C52103X",
      "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE " &
      "ASSIGNMENTS, THE LENGTHS MUST MATCH; ALSO " &
      "CHECK WHETHER CONSTRAINT_ERROR " &
      "OR STORAGE_ERROR ARE RAISED FOR LARGE ARRAYS");

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

   --    (4) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
   --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
   --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
   --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)

   Constr_Err :         -- THIS BLOCK CATCHES CONSTRAINT_ERROR
   -- FOR THE TYPE DECLARATION.
   begin

      Dcl_Arr :
      declare   -- THIS BLOCK DECLARES THE ARRAY TYPE

         type Ta42 is
           array
             (Integer range Ident_Int (-2) ..
                  Ident_Int (Integer'Last)) of Boolean;
         -- CONSTRAINT_ERROR MAY BE RAISED BY THE ARRAY TYPE DECLARATION.
         pragma Pack (Ta42);

         subtype Ta41 is Ta42;

      begin

         Comment
           ("NO CONSTRAINT_ERROR FOR TYPE " &
            "WITH 'LENGTH = INTEGER'LAST + 3");

         Obj_Dcl :
         declare   -- THIS BLOCK DECLARES TWO BOOLEAN ARRAYS THAT
            -- HAVE INTEGER'LAST + 3 COMPONENTS; STORAGE_ERROR MAY BE RAISED.
            Arr41 : Ta41;
            Arr42 : Ta42;

         begin

            Comment
              ("NO STORAGE_ERROR OR CONSTRAINT_ERROR RAISED " &
               "WHEN ALLOCATING TWO BIG BOOLEAN ARRAYS");
            -- INITIALIZATION OF RHS ARRAY:

            -- ONLY A SHORT INITIAL SEGMENT IS INITIALIZED, SINCE A COMPLETE
            -- INITIALIZATION MIGHT TAKE TOO LONG AND THE EXECUTION MIGHT BE
            -- ABORTED BEFORE THE LENGTH COMPARISON OF THE ARRAY ASSIGNMENT
            -- IS ATTEMPTED.

            No_Excp :
            begin     -- NO EXCEPTION SHOULD OCCUR HERE.
               for I in Ident_Int (-2) .. Ident_Int (2) loop
                  Arr41 (I) := False;  -- VALUES ARE:: FTFFT
               end loop;

               Arr41 (-1) := True;

               Arr41 (2) := True;          -- RHS IS: F T F F T

               -- INITIALIZATION OF UNUSED COMPONENT OF LHS ARRAY:

               Arr42 (-2) := True;

            exception

               when Constraint_Error =>
                  Failed
                    ("CONSTRAINT_ERROR RAISED WHEN " &
                     "ASSIGNING TO ARRAY COMPONENTS");
               when others =>
                  Failed ("OTHER EXCEPTION RAISED - 1");

            end No_Excp;

            Do_Slice :
            begin
               -- SLICE ASSIGNMENT:

               Arr42 (Ident_Int (-1) .. Ident_Int (Integer'Last)) :=
                 Arr41 (Ident_Int (-2) .. Ident_Int (Integer'Last - 1));

               Comment ("NO EXCEPTION RAISED DURING SLICE " & "ASSIGNMENT");

               -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

               Chk_Slice :
               begin
                  for I in Ident_Int (-1) .. Ident_Int (2) loop

                     if Arr42 (I) /= False and I /= 0 then
                        Failed ("SLICE ASSIGNMENT NOT " & "CORRECT (VALUES)");
                     elsif Arr42 (I) /= True and I = 0 then
                        Failed ("SLICE ASSIGNMENT NOT " & "CORRECT (VALUES)");
                     end if;

                  end loop;

                  if Arr42 (-2) /= True then
                     Failed ("SLICE ASSIGNMENT NOT CORRECT " & "(SLIDING)");
                  end if;

               exception

                  when others =>
                     Failed ("SOME EXCEPTION RAISED - 2");

               end Chk_Slice;

            exception

               when Constraint_Error =>
                  Comment
                    ("CONSTRAINT_ERROR RAISED DURING " & "SLICE ASSIGNMENT");
               when Storage_Error =>
                  Comment
                    ("STORAGE_ERROR RAISED DURING SLICE " & "ASSIGNMENT");
               when others =>
                  Failed ("SOME EXCEPTION DURING SLICE " & "ASSIGNMENT");
            end Do_Slice;

         end Obj_Dcl;

      exception

         when Storage_Error =>
            Comment
              ("STORAGE_ERROR RAISED WHEN DECLARING " &
               "TWO PACKED BOOLEAN ARRAYS WITH " &
               "INTEGER'LAST + 3 COMPONENTS");
         when Constraint_Error =>
            Comment
              ("CONSTRAINT_ERROR RAISED WHEN DECLARING " &
               "TWO PACKED BOOLEAN ARRAYS WITH " &
               "INTEGER'LAST + 3 COMPONENTS");
         when others =>
            Failed ("SOME EXCEPTION RAISED - 3");

      end Dcl_Arr;

   exception

      when Constraint_Error =>
         Comment
           ("CONSTRAINT_ERROR RAISED WHEN DECLARING AN " &
            "ARRAY TYPE WITH INTEGER'LAST + 3 COMPONENTS");

      when Storage_Error =>
         Failed ("STORAGE_ERROR RAISED FOR TYPE DECLARATION");

      when others =>
         Failed ("OTHER EXCEPTION RAISED - 4");

   end Constr_Err;

   Result;

end C52103x;
