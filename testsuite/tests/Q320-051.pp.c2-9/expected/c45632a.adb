-- C45632A.ADA

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
--     CHECK THAT FOR PREDEFINED TYPE INTEGER, CONSTRAINT_ERROR
--     IS RAISED FOR ABS (INTEGER'FIRST) IF
--     -INTEGER'LAST > INTEGER'FIRST.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- HISTORY:
--     RJW 02/10/86  CREATED ORIGINAL TEST.
--     JET 12/30/87  UPDATED HEADER FORMAT AND ADDED CODE TO
--                   PREVENT OPTIMIZATION.
--     MRM 03/30/93  REMOVED NUMERIC ERROR FOR 9X COMPATIBILITY

with Report; use Report;

procedure C45632a is

   I : Integer := Ident_Int (Integer'First);

begin

   Test
     ("C45632A",
      "CHECK THAT FOR PREDEFINED TYPE INTEGER " &
      "CONSTRAINT_ERROR IS RAISED " &
      "FOR ABS (INTEGER'FIRST) IF -INTEGER'LAST > " & "INTEGER'FIRST");

   begin
      if -Integer'Last > Integer'First then
         begin
            if Equal (abs I, I) then
               null;
            else
               Failed ("WRONG RESULT FOR ABS");
            end if;
            Failed ("EXCEPTION NOT RAISED");
         exception
            when Constraint_Error =>
               Comment ("CONSTRAINT_ERROR RAISED");
            when others =>
               Failed ("WRONG EXCEPTION RAISED");
         end;
      else
         Comment ("-INTEGER'LAST <= INTEGER'FIRST");
      end if;
   end;

   Result;

end C45632a;