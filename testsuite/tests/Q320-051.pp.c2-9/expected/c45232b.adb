-- C45232B.ADA

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
-- CHECK THAT NO EXCEPTION IS RAISED WHEN AN INTEGER LITERAL IN A COMPARISON
-- BELONGS TO THE BASE TYPE BUT IS OUTSIDE THE SUBTYPE OF THE OTHER OPERAND.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X

-- P. BRASHEAR  08/21/86
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

with Report, System;
use Report;
procedure C45232b is

begin

   Test
     ("C45232B",
      "NO EXCEPTION IS RAISED WHEN AN INTEGER " &
      "LITERAL IN A COMPARISON BELONGS TO THE BASE " &
      "TYPE BUT IS OUTSIDE THE SUBTYPE OF THE " & "OTHER OPERAND");

   declare

      type Int10 is range -10 .. 5;

   begin

      if 7 > Int10'(-10) then
         Comment ("NO EXCEPTION RAISED FOR '7 > " & "INT10'(-10)'");
      else
         Failed ("WRONG RESULT FOR '7 > INT10'(-10)'");
      end if;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR '7 " & "> INT10'(-10)'");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR '7 > " & "INT10'(-10)'");
   end;

   declare

      type Int10 is range -10 .. 5;

   begin

      if 7 not in Int10 then
         Comment ("NO EXCEPTION RAISED FOR '7 NOT IN " & "INT'");
      else
         Failed ("WRONG RESULT FOR '7 NOT IN INT'");
      end if;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR '7 " & "NOT IN INT'");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR '7 NOT IN " & "INT'");
   end;

   declare

      type Int700 is range -700 .. 500;

   begin
      if 600 > Int700'(5) then
         Comment ("NO EXCEPTION RAISED FOR '600 > " & "INT700'(5)'");
      else
         Failed ("WRONG RESULT FOR '600 > INT700'(5)'");
      end if;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR '600 " & "> INT700'(5)'");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR '600 > " & "INT700'(5)'");
   end;

   declare

      type Int700 is range -700 .. 500;

   begin

      if 600 not in Int700 then
         Comment ("NO EXCEPTION RAISED FOR '600 NOT IN " & "INT700'");
      else
         Failed ("WRONG RESULT FOR '600 NOT IN INT700'");
      end if;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR '600 " & "NOT IN INT700'");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR '600 NOT IN " & "INT700'");
   end;

   Result;

end C45232b;
