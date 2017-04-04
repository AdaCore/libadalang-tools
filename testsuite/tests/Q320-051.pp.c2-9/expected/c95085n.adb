-- C95085N.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED AFTER AN ENTRY CALL FOR THE CASE OF
-- A PRIVATE TYPE IMPLEMENTED AS A SCALAR TYPE WHERE THE VALUE OF THE FORMAL
-- PARAMETER DOES NOT BELONG TO THE SUBTYPE OF THE ACTUAL PARAMETER.

-- JWC 10/29/85
-- JRK 1/15/86 ENSURE THAT EXCEPTION RAISED AFTER CALL, NOT BEFORE
--                  CALL.

with Report; use Report;
procedure C95085n is

begin
   Test
     ("C95085N",
      "CHECK THAT PRIVATE TYPE (SCALAR) RAISES " &
      "CONSTRAINT_ERROR AFTER CALL WHEN FORMAL " &
      "PARAMETER VALUE IS NOT IN ACTUAL'S SUBTYPE");

   declare

      Called : Boolean := False;

      package P is
         type T is private;
         Dc : constant T;

         generic
         package Pp is
         end Pp;
      private
         type T is new Integer;
         Dc : constant T := -1;
      end P;

      task Tsk is
         entry E (X : out P.T);
      end Tsk;

      task body Tsk is
      begin
         select
            accept E (X : out P.T) do
               Called := True;
               X      := P.Dc;
            end E;
         or
            terminate;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK BODY");
      end Tsk;

      generic
         Y : in out P.T;
      package Call is
      end Call;

      package body Call is
      begin
         Tsk.E (Y);
         Failed ("EXCEPTION NOT RAISED AFTER RETURN");
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED");
      end Call;

      package body P is
         Z : T range 0 .. 1 := 0;
         package body Pp is
            package Call_Q is new Call (Z);
         end Pp;
      end P;

   begin

      begin
         declare
            package Call_Q_Now is new P.Pp;    -- START HERE.
         begin
            null;
         end;
      exception
         when others =>
            Failed ("WRONG HANDLER INVOKED");
      end;

   end;

   Result;
end C95085n;
