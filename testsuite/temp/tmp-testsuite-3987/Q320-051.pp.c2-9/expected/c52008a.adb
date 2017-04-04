-- C52008A.ADA

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
-- CHECK THAT A RECORD VARIABLE CONSTRAINED BY A SPECIFIED DISCRIMINANT
-- VALUE CANNOT HAVE ITS DISCRIMINANT VALUE ALTERED BY ASSIGNMENT.
-- ASSIGNING AN ENTIRE RECORD VALUE WITH A DIFFERENT DISCRIMINANT VALUE
-- SHOULD RAISE CONSTRAINT_ERROR AND LEAVE THE TARGET VARIABLE
-- UNALTERED.  THIS TEST USES STATIC DISCRIMINANT VALUES.

-- ASL 6/25/81
-- SPS 3/21/83

with Report;
procedure C52008a is

   use Report;

   type Rec (Disc : Integer) is record
      Comp : Integer;
   end record;

   R : Rec (5) := (5, 0);

begin

   Test
     ("C52008A",
      "CANNOT ASSIGN RECORD VARIABLE WITH SPECIFIED " &
      "DISCRIMINANT VALUE A VALUE WITH A DIFFERENT " &
      "STATIC DISCRIMINANT VALUE");

   begin
      R := (Disc => 5, Comp => 3);
      if R /= (5, 3) then
         Failed ("LEGAL ASSIGNMENT FAILED");
      end if;
      R := (Disc => 4, Comp => 2);
      Failed ("RECORD ASSIGNED VALUE WITH DIFFERENT DISCRIMINANT " & "VALUE");
   exception
      when Constraint_Error =>
         if R /= (5, 3) then
            Failed
              ("TARGET RECORD VALUE ALTERED BY " &
               "ASSIGNMENT TO VALUE WITH DIFFERENT " &
               "DISCRIMINANT VALUE EVEN AFTER " &
               "CONSTRAINT_ERROR RAISED");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION");
   end;

   Result;

end C52008a;
