-- C45252A.ADA

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
-- FOR FIXED POINT TYPES, CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN A LITERAL
-- USED IN A COMPARISON OR MEMBERSHIP OPERATION (AS THE FIRST OPERAND) DOES NOT
-- BELONG TO THE BASE TYPE.
--
-- CHECK THAT NO EXCEPTION IS RAISED FOR A FIXED POINT RELATIONAL OR MEMBERSHIP
-- OPERATION IF LITERAL VALUES BELONG TO THE BASE TYPE.

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X

-- WRG 9/10/86
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

with Report; use Report;
procedure C45252a is

   -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S 'MANTISSA VALUE.

   type Middle_M3 is delta 0.5 range 0.0 .. 2.5;
   type Like_Duration_M23 is delta 0.020 range -86_400.0 .. 86_400.0;

begin

   Test
     ("C45252A",
      "CHECK RAISING OF EXCEPTIONS BY RELATIONAL " &
      "OPERATIONS FOR FIXED POINT TYPES - BASIC TYPES");

   -------------------------------------------------------------------

   begin
      -- 2.0 ** 31 < 2.9E9 < 2.0 ** 32.
      if 2.9E9 <= Like_Duration_M23'Last then
         Failed ("2.9E9 <= LIKE_DURATION_M23'LAST");
      end if;
   exception
      when Constraint_Error =>
         Comment
           ("CONSTRAINT_ERROR RAISED BY COMPARISON " &
            """2.9E9 <= LIKE_DURATION_M23'LAST""");
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED BY COMPARISON " &
            """2.9E9 <= LIKE_DURATION_M23'LAST""");
   end;

   -------------------------------------------------------------------

   begin
      -- 2.0 ** 63 < 1.0E19 < 2.0 ** 64.
      if 1.0E19 in Like_Duration_M23 then
         Failed ("1.0E19 IN LIKE_DURATION_M23");
      end if;
   exception
      when Constraint_Error =>
         Comment
           ("CONSTRAINT_ERROR RAISED BY MEMBERSHIP TEST " &
            """1.0E19 IN LIKE_DURATION_M23""");
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED BY MEMBERSHIP TEST " &
            """1.0E19 IN LIKE_DURATION_M23""");
   end;

   -------------------------------------------------------------------

   begin
      -- 2.0 ** 63 < 1.0E19 < 2.0 ** 64.
      if 1.0E19 <= Middle_M3'Last then
         Failed ("1.0E19 <= MIDDLE_M3'LAST");
      end if;
   exception
      when Constraint_Error =>
         Comment
           ("CONSTRAINT_ERROR RAISED BY COMPARISON " &
            """1.0E19 <= MIDDLE_M3'LAST""");
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED BY COMPARISON " &
            """1.0E19 <= MIDDLE_M3'LAST""");
   end;

   -------------------------------------------------------------------

   begin
      -- 2.0 ** 31 < 2.9E9 < 2.0 ** 32.
      if 2.9E9 in Middle_M3 then
         Failed ("2.9E9 IN MIDDLE_M3");
      end if;
   exception
      when Constraint_Error =>
         Comment
           ("CONSTRAINT_ERROR RAISED BY MEMBERSHIP TEST " &
            """2.9E9 IN MIDDLE_M3""");
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED BY MEMBERSHIP TEST " &
            """2.9E9 IN MIDDLE_M3""");
   end;

   -------------------------------------------------------------------

   begin
      -- 3.5 IS A MODEL NUMBER OF THE TYPE MIDDLE_M3.
      if 3.5 <= Middle_M3'Last then
         Failed ("3.5 <= MIDDLE_M3'LAST");
      end if;
   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED BY COMPARISON " &
            """3.5 <= MIDDLE_M3'LAST""");
      when others =>
         Failed
           ("SOME EXCEPTION RAISED BY COMPARISON " &
            """3.5 <= MIDDLE_M3'LAST""");
   end;

   -------------------------------------------------------------------

   begin
      if 3.0 in Middle_M3 then
         Failed ("3.0 IN MIDDLE_M3");
      end if;
   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED BY MEMBERSHIP TEST " &
            """3.0 IN MIDDLE_M3""");
      when others =>
         Failed
           ("SOME EXCEPTION RAISED BY MEMBERSHIP TEST " &
            """3.0 IN MIDDLE_M3""");
   end;

   -------------------------------------------------------------------

   begin
      if 86_450.0 <= Like_Duration_M23'Last then
         Failed ("86_450.0 <= LIKE_DURATION_M23'LAST");
      end if;
   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED BY COMPARISON " &
            """86_450.0 <= LIKE_DURATION_M23'LAST""");
      when others =>
         Failed
           ("SOME EXCEPTION RAISED BY COMPARISON " &
            """86_450.0 <= LIKE_DURATION_M23'LAST""");
   end;

   -------------------------------------------------------------------

   begin
      if 86_500.0 in Like_Duration_M23 then
         Failed ("86_500.0 IN LIKE_DURATION_M23");
      end if;
   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED BY MEMBERSHIP TEST " &
            """86_500.0 IN LIKE_DURATION_M23""");
      when others =>
         Failed
           ("SOME EXCEPTION RAISED BY MEMBERSHIP TEST " &
            """86_500.0 IN LIKE_DURATION_M23""");
   end;

   -------------------------------------------------------------------

   begin
      if -86_450.0 in Like_Duration_M23 then
         Failed ("-86_450.0 IN LIKE_DURATION_M23");
      end if;
   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED BY MEMBERSHIP TEST " &
            """-86_450.0 IN LIKE_DURATION_M23""");
      when others =>
         Failed
           ("SOME EXCEPTION RAISED BY MEMBERSHIP TEST " &
            """-86_450.0 IN LIKE_DURATION_M23""");
   end;

   -------------------------------------------------------------------

   Result;

end C45252a;
