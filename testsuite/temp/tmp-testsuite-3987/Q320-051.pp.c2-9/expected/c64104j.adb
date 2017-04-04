-- C64104J.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER THE APPROPRIATE
--     CIRCUMSTANCES FOR ACCESS PARAMETERS, NAMELY WHEN THE
--     ACTUAL INDEX BOUNDS OR DISCRIMINANTS ARE NOT EQUAL
--     TO THE FORMAL CONSTRAINTS BEFORE THE CALL (FOR IN AND IN OUT
--     MODES), AND WHEN THE FORMAL CONSTRAINTS ARE NOT EQUAL TO THE
--     ACTUAL CONSTRAINTS UPON RETURN (FOR IN OUT AND OUT MODES).

--         (G) AFTER RETURN, OUT MODE, UNCONSTRAINED FORMAL, STATIC ONE
--             DIMENSIONAL BOUNDS.

-- HISTORY:
--     JRK 03/18/81  CREATED ORIGINAL TEST.
--     NL  10/13/81
--     BCB 11/12/87  CHANGED HEADING TO STANDARD FORMAT.  ADDED CODE TO
--                   ENSURE THAT SUBPROGRAMS ARE ACTUALLY CALLED.

with Report;
procedure C64104j is

   use Report;

begin
   Test
     ("C64104J",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
      "APPROPRIATELY FOR ACCESS PARAMETERS");

   --------------------------------------------------

   declare

      type A is access String;

      Called : Boolean := False;

      V : A (1 .. 3) := new String (1 .. 3);

      procedure P (X : out A) is
      begin
         Called := True;
         X      := new String (2 .. 3);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE");
      end P;

   begin

      P (V);
      Failed ("EXCEPTION NOT RAISED AFTER RETURN");

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("SUBPROGRAM P WAS NOT CALLED");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end;

   --------------------------------------------------

   Result;

end C64104j;
