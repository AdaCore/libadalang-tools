-- C52009B.ADA

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
-- CHECK THAT A RECORD VARIABLE DESIGNATED BY AN ACCESS VALUE CANNOT
-- HAVE ITS DISCRIMINANT ALTERED, EVEN BY A COMPLETE RECORD
-- ASSIGNMENT, AND EVEN THOUGH THE THE TARGET ACCESS VARIABLE IS NOT
-- CONSTRAINED TO A SPECIFIC DISCRIMINANT VALUE.  ATTEMPTING TO
-- CHANGE THE TARGET'S DISCRIMINANT RAISES CONSTRAINT_ERROR AND LEAVES
-- THE TARGET RECORD UNALTERED.  THIS TEST USES NON-STATIC DISCRIMINANT
-- VALUES AND A TYPE WITH DEFAULT DISCRIMINANTS.

-- ASL 7/6/81
-- SPS 10/26/82
-- JBG 1/10/84

with Report;
procedure C52009b is

   use Report;

   type Rec (Disc : Integer := 5) is record
      Comp : Integer := 0;
   end record;

   type Rec_Name is access Rec;

   Hr : Rec_Name := new Rec;

begin

   Test
     ("C52009B",
      "CANNOT CHANGE, THROUGH ASSIGNMENT, THE " &
      "(DYNAMIC) DISCRIMINANT VALUE OF A RECORD DESIGNATED " &
      "BY AN ACCESS VALUE");

   begin
      Hr.all := (Disc => Ident_Int (5), Comp => 3);
      if Hr.all /= (Ident_Int (5), 3) then
         Failed ("LEGAL ASSIGNMENT FAILED");
      end if;
   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED WHEN DISCRIMINANT " &
            "VALUE NOT CHANGED");
   end;

   begin
      Hr.all := (Disc => Ident_Int (4), Comp => 2);
      Failed ("RECORD ASSIGNED VALUE WITH DIFFERENT DISCRIMINANT " & "VALUE");
   exception
      when Constraint_Error =>
         Comment ("DETECTED ATTEMPT TO CHANGE DISCRIMINANT " & "VALUE");
      when others =>
         Failed ("WRONG EXCEPTION");
   end;

   Result;

end C52009b;
