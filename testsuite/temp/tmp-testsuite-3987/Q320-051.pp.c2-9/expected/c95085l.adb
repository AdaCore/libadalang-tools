-- C95085L.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER THE APPROPRIATE
-- CIRCUMSTANCES FOR ACCESS PARAMETERS IN ENTRY CALLS, NAMELY WHEN
-- THE ACTUAL INDEX BOUNDS OR DISCRIMINANTS ARE NOT EQUAL
-- TO THE FORMAL CONSTRAINTS BEFORE THE CALL (FOR IN AND IN OUT
-- MODES), AND WHEN THE FORMAL CONSTRAINTS ARE NOT EQUAL TO THE
-- ACTUAL CONSTRAINTS UPON RETURN (FOR IN OUT AND OUT MODES).

--       (I) AFTER RETURN, OUT MODE, CONSTRAINED FORMAL, STATIC
--           PRIVATE DISCRIMINANTS.

-- JWC 10/24/85

with Report; use Report;
procedure C95085l is

begin
   Test
     ("C95085L",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
      "APPROPRIATELY FOR ACCESS PARAMETERS");

   --------------------------------------------------

   declare

      Called : Boolean := False;

      package Pkg is
         type E is (E1, E2, E3);
         type T (D : E := E1; B : Boolean := False) is private;
      private
         type Arr is array (E range <>) of Integer;
         type T (D : E := E1; B : Boolean := False) is record
            I : Integer;
            case B is
               when False =>
                  J : Integer;
               when True =>
                  A : Arr (E1 .. D);
            end case;
         end record;
      end Pkg;
      use Pkg;

      type A is access T;
      subtype Sa is A (E2, True);
      V : A (E2, False) := new T (E2, False);

      task Tsk is
         entry E (X : out Sa);
      end Tsk;

      task body Tsk is
      begin
         select
            accept E (X : out Sa) do
               Called := True;
               X      := new T (E2, True);
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
      Failed ("EXCEPTION NOT RAISED AFTER RETURN");

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end;

   ------------------------------------------------

   Result;
end C95085l;
