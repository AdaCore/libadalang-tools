-- C45252B.ADA

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
-- OBJECTIVE:
--     CHECK THAT NO EXCEPTION IS RAISED WHEN A FIXED POINT LITERAL
--     OPERAND IN A COMPARISON OR A FIXED POINT LITERAL LEFT OPERAND
--     IN A MEMBERSHIP TEST BELONGS TO THE BASE TYPE BUT IS OUTSIDE
--     THE RANGE OF THE SUBTYPE.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X

-- HISTORY:
--     PWB 09/04/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.
--     JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

with Report, System;
use Report;
procedure C45252b is

begin

   Test
     ("C45252B",
      "NO EXCEPTION IS RAISED WHEN A FIXED " &
      "LITERAL USED IN A COMPARISON OR AS THE " &
      "LEFT OPERAND IN A MEMBERSHIP TEST " &
      "BELONGS TO THE BASE TYPE BUT IS OUTSIDE " &
      "THE RANGE OF THE SUBTYPE");

   declare
      type Fixed is delta 0.25 range -10.0 .. 10.0;
      subtype Fixed_1 is Fixed range -1.0 .. 1.0;
      Num : Fixed_1 := 0.0;
   begin    -- FIXED COMPARISON

      if Equal (3, 3) then
         Num := Fixed_1'(0.5);
      end if;

      if 2.0 > Num then
         Comment ("NO EXCEPTION RAISED FOR FIXED " & "COMPARISON");
      else
         Failed ("WRONG RESULT FROM FIXED " & "COMPARISON");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR " & "FIXED COMPARISON");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR " & "FIXED COMPARISON");
   end;  -- FIXED COMPARISON

   declare
      type Fixed is delta 0.25 range -10.0 .. 10.0;
      subtype Fixed_1 is Fixed range -1.0 .. 1.0;
   begin    -- FIXED MEMBERSHIP

      if 2.0 in Fixed_1 then
         Failed ("WRONG RESULT FROM FIXED " & "MEMBERSHIP");
      else
         Comment ("NO EXCEPTION RAISED FOR FIXED " & "MEMBERSHIP");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR  " & "FIXED MEMBERSHIP");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR  " & "FIXED MEMBERSHIP");
   end;  -- FIXED MEMBERSHIP

   declare -- PRECISE FIXED COMPARISON
      type Fine_Fixed is delta System.Fine_Delta range -1.0 .. 1.0;
      subtype Sub_Fine is Fine_Fixed range -0.5 .. 0.5;
      Num : Sub_Fine := 0.0;
   begin
      if Equal (3, 3) then
         Num := 0.25;
      end if;

      if 0.75 > Num then
         Comment ("NO EXCEPTION RAISED FOR FINE_FIXED " & "COMPARISON");
      else
         Failed ("WRONG RESULT FROM FINE_FIXED COMPARISON");
      end if;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR " & "FINE_FIXED COMPARISON");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR  " & "FINE_FIXED COMPARISON");
   end;  --  FINE_FIXED COMPARISON

   declare -- PRECISE FIXED MEMBERSHIP
      type Fine_Fixed is digits System.Max_Digits;
      subtype Sub_Fine is Fine_Fixed range -0.5 .. 0.5;
   begin

      if 0.75 in Sub_Fine then
         Failed ("WRONG RESULT FROM FINE_FIXED MEMBERSHIP");
      else
         Comment ("NO EXCEPTION RAISED FOR FINE_FIXED " & "MEMBERSHIP");
      end if;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR " & "FINE_FIXED MEMBERSHIP");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR  " & "FINE_FIXED MEMBERSHIP");
   end;  --  FINE_FIXED MEMBERSHIP

   Result;

end C45252b;
