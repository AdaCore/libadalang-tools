-- C37211D.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED BY A DISCRIMINANT CONSTRAINT IF
-- A VALUE SPECIFIED FOR A DISCRIMINANT DOES NOT LIE IN THE RANGE OF THE
-- DISCRIMINANT. THIS TEST CONTAINS CHECKS FOR SUBTYPE INDICATIONS WHERE
-- THE TYPE MARK DENOTES AN INCOMPLETE TYPE.

-- R.WILLIAMS 8/28/86
-- EDS 7/14/98 AVOID OPTIMIZATION

with Report; use Report;
procedure C37211d is

   Global : Boolean;

   type Day is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

   subtype Weekday is Day range Mon .. Fri;

   function Switch (B : Boolean) return Boolean is
   begin
      Global := B;
      return B;
   end Switch;

   function Ident (D : Day) return Day is
   begin
      return Day'Val (Ident_Int (Day'Pos (D)));
   end Ident;

begin
   Test
     ("C37211D",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
      "A DISCRIMINANT CONSTRAINT IF A VALUE " &
      "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
      "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
      "TYPE MARK DENOTES AN INCOMPLETE TYPE");

   begin
      declare

         B1 : Boolean := Switch (True);

         type Rec (D : Weekday);

         type Accrec is access Rec (Ident (Sun));

         B2 : Boolean := Switch (False);

         type Rec (D : Weekday) is record
            null;
         end record;
      begin
         declare
            Ac : Accrec;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE ACCREC " &
               Day'Image (Ac.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT AC");
      end;

   exception
      when Constraint_Error =>
         if Global then
            null;
         else
            Failed
              ("EXCEPTION RAISED AT ELABORATION OF " &
               "FULL TYPE REC NOT TYPE ACCREC");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE ACCREC");
   end;

   Result;
end C37211d;
