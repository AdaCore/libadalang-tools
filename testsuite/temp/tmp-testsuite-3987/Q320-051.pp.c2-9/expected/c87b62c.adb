-- C87B62C.ADA

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
--     CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
--       IN A LENGTH CLAUSE THAT SPECIFIES 'SMALL, THE EXPRESSION
--       MUST BE OF SOME REAL TYPE.

-- HISTORY:
--     TRH 09/08/82  CREATED ORIGINAL TEST.
--     PWB 02/19/86  ADDED COMMENTS TO CLARIFY NON-APPLICABILITY;
--                   REMOVED TEXT NOT RELATED TO TEST OBJECTIVE.
--     BCB 01/04/88  MODIFIED HEADER.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;

procedure C87b62c is

   type Pos_Int is new Integer range 1 .. Integer'Last;
   type Pos_Fix is delta 0.1 range 0.0 .. 10.0;
   Err : Boolean := False;

   function "+" (X : Pos_Int) return Pos_Fix is
   begin
      Err := True;
      return Pos_Fix (X);
   end "+";

   function "+" (X : Pos_Fix) return Pos_Int is
   begin
      Err := True;
      return Pos_Int (X);
   end "+";

begin
   Test
     ("C87B62C",
      "OVERLOADED EXPRESSION WITHIN LENGTH CLAUSE " &
      "- SPECIFICATION OF ATTRIBUTE T'SMALL");

   declare
      type Just_Like_Fixed is delta 0.1 range -1.0 .. 1.0;
      type Fixed is delta 0.1 range -1.0 .. 1.0;

      Fikst_Small : constant := Just_Like_Fixed'Small;
      type Check is delta 0.1 range -1.0 .. 1.0;

      for Check'Small use Fikst_Small;
      for Fixed'Small use +Fikst_Small;

   begin
      if Err then
         Failed
           ("RESOLUTION INCORRECT FOR EXPRESSION IN " &
            "LENGTH CLAUSE USING 'SMALL");
      end if;
   end;

   Result;
end C87b62c;
