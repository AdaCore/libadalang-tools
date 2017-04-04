-- C95085E.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER THE APPROPRIATE CIRCUMSTANCES
-- FOR ACCESS PARAMETERS IN ENTRY CALLS, NAMELY WHEN THE ACTUAL INDEX BOUNDS OR
-- DISCRIMINANTS ARE NOT EQUAL TO THE FORMAL CONSTRAINTS BEFORE THE CALL (FOR
-- IN AND IN OUT MODES), AND WHEN THE FORMAL CONSTRAINTS ARE NOT EQUAL TO THE
-- ACTUAL CONSTRAINTS UPON RETURN (FOR IN OUT AND OUT MODES).

--       (B) BEFORE CALL, IN MODE, DYNAMIC TWO DIMENSIONAL BOUNDS.

-- JWC 10/23/85

with Report; use Report;
procedure C95085e is

begin
   Test
     ("C95085E",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
      "APPROPRIATELY FOR ACCESS PARAMETERS");

   --------------------------------------------------

   declare

      type T is array (Boolean range <>, Character range <>) of Integer;

      type A is access T;
      subtype A1 is A (Boolean, 'A' .. 'C');
      V : A := new T (Boolean, 'A' .. Ident_Char ('B'));

      task Tsk is
         entry E (X : A1);
      end Tsk;

      task body Tsk is
      begin
         select
            accept E (X : A1) do
               Failed ("EXCEPTION NOT RAISED ON CALL");
            end E;
         or
            terminate;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK BODY");
      end Tsk;

   begin

      Tsk.E (V);
      Failed ("EXCEPTION NOT RAISED BEFORE CALL");

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end;

   --------------------------------------------------

   Result;
end C95085e;
