-- C64104N.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED AT THE PLACE OF THE CALL
--     FOR THE CASE OF A PRIVATE TYPE IMPLEMENTED AS A SCALAR TYPE
--     WHERE THE VALUE OF THE FORMAL PARAMETER DOES NOT BELONG TO THE
--     SUBTYPE OF THE ACTUAL PARAMETER.

-- HISTORY:
--     DAVID A. TAFFS
--     CPP 07/23/84
--     RDH 04/18/90  REVISED TO CHECK THAT SUBPROGRAM IS ACTUALLY
--                   CALLED.
--     THS 09/21/90  REWORDED COMMENT STATING THAT THE TEST DOES NOT
--                   ACCEPT THE LITERAL INTERPRETATION OF 6.4.1(9).

with Report; use Report;
procedure C64104n is

begin
   Test
     ("C64104N",
      "CHECK THAT PRIVATE TYPE (SCALAR) RAISES " &
      "CONSTRAINT_ERROR WHEN ACTUAL AND FORMAL PARAMETER " & "BOUNDS DIFFER");

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

      procedure Q (X : in out P.T) is
      begin
         Called := True;
         X      := P.Dc;
         if P."=" (X, P.Dc) then
            Comment ("PROCEDURE Q WAS CALLED");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED INSIDE SUBPROGRAM");
      end Q;

      generic
         Y : in out P.T;
      package Call is
      end Call;

      package body Call is
      begin
         Q (Y);
      end Call;

-- NOTE CALL HAS VARIABLE OF A PRIVATE TYPE AS AN OUT PARAMETER. THIS TEST
-- DOES NOT ACCEPT THE LITERAL INTERPRETATION OF 6.4.1(9). REFER TO ADA
-- IMPLEMENTOR'S GUIDE 6.4.1 SEMANTIC RAMIFICATION 19 AND AI-00025 FOR
-- CLARIFICATION AS TO WHY THE LITERAL INTERPRETATION IS REJECTED.

      package body P is
         Z : T range 0 .. 1 := 0;
         package body Pp is
            package Call_Q is new Call (Z);
         end Pp;
      end P;

   begin
      begin
         declare
            package Call_Q_Now is new P.Pp;    -- EXCEPTION
         begin
            Failed ("NO EXCEPTION RAISED");
         end;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("SUBPROGRAM Q WAS NOT CALLED");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED");
      end;

      Result;

   end;
end C64104n;
