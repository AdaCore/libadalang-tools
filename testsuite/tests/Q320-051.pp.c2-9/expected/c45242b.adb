-- C45242B.ADA

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
--     CHECK THAT NO EXCEPTION IS RAISED WHEN A FLOATING POINT LITERAL
--     OPERAND IN A COMPARISON OR A FLOATING POINT LITERAL LEFT OPERAND
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
procedure C45242b is

begin

   Test
     ("C45242B",
      "NO EXCEPTION IS RAISED WHEN A FLOATING " &
      "LITERAL USED IN A COMPARISON OR AS THE " &
      "LEFT OPERAND IN A MEMBERSHIP TEST " &
      "BELONGS TO THE BASE TYPE BUT IS OUTSIDE " & "THE RANGE OF THE SUBTYPE");

   declare
      N : Float := Float (Ident_Int (1));
      subtype Float_1 is Float range -1.0 .. N;
      Num : Float_1 := N;
   begin    -- PRE-DEFINED FLOAT COMPARISON

      if Equal (3, 3) then
         Num := Float_1'(0.5);
      end if;

      if 2.0 > Num then
         Comment ("NO EXCEPTION RAISED FOR PRE-DEFINED FLOAT " & "COMPARISON");
      else
         Failed ("WRONG RESULT FROM PRE-DEFINED FLOAT " & "COMPARISON");
      end if;
   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED FOR PRE-DEFINED " & "FLOAT COMPARISON");
      when others =>
         Failed
           ("OTHER EXCEPTION RAISED FOR PRE-DEFINED " & "FLOAT COMPARISON");
   end;  -- PRE-DEFINED FLOAT COMPARISON

   declare
      N : Float := Float (Ident_Int (1));
      subtype Float_1 is Float range -1.0 .. N;
   begin    -- PRE-DEFINED FLOAT MEMBERSHIP

      if 2.0 in Float_1 then
         Failed ("WRONG RESULT FROM PRE-DEFINED FLOAT " & "MEMBERSHIP");
      else
         Comment ("NO EXCEPTION RAISED FOR PRE-DEFINED FLOAT " & "MEMBERSHIP");
      end if;
   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED FOR PRE-DEFINED " & "FLOAT MEMBERSHIP");
      when others =>
         Failed
           ("OTHER EXCEPTION RAISED FOR PRE-DEFINED " & "FLOAT MEMBERSHIP");
   end;  -- PRE-DEFINED FLOAT MEMBERSHIP

   declare -- PRECISE FLOAT COMPARISON
      type Fine_Float is digits System.Max_Digits;
      N : Fine_Float := 0.5 * Fine_Float (Ident_Int (1));
      subtype Sub_Fine is Fine_Float range -0.5 .. N;
      Num : Sub_Fine := N;
   begin
      if Equal (3, 3) then
         Num := 0.25;
      end if;

      if 0.75 > Num then
         Comment ("NO EXCEPTION RAISED FOR FINE_FLOAT " & "COMPARISON");
      else
         Failed ("WRONG RESULT FROM FINE_FLOAT COMPARISON");
      end if;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR " & "FINE_FLOAT COMPARISON");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR  " & "FINE_FLOAT COMPARISON");
   end;  --  FINE_FLOAT COMPARISON

   declare -- PRECISE FLOAT MEMBERSHIP
      type Fine_Float is digits System.Max_Digits;
      N : Fine_Float := 0.5 * Fine_Float (Ident_Int (1));
      subtype Sub_Fine is Fine_Float range -0.5 .. N;
   begin

      if 0.75 in Sub_Fine then
         Failed ("WRONG RESULT FROM FINE_FLOAT MEMBERSHIP");
      else
         Comment ("NO EXCEPTION RAISED FOR FINE_FLOAT " & "MEMBERSHIP");
      end if;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR " & "FINE_FLOAT MEMBERSHIP");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR  " & "FINE_FLOAT MEMBERSHIP");
   end;  --  FINE_FLOAT MEMBERSHIP

   Result;

end C45242b;
