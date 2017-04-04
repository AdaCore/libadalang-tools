-- C87B07E.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- FOR THE ATTRIBUTE OF THE FORM T'IMAGE (X), THE OPERAND X MUST BE OF TYPE T.
-- THE RESULT IS OF THE PREDEFINED TYPE STRING.

-- TRH  15 SEPT 82

with Report; use Report;

procedure C87b07e is

   type New_Int is new Integer;
   type Number is new Integer;
   type New_Str is new String;

   function "+" (X : New_Int) return New_Int renames "-";
   function "-" (X : Number) return Number renames "+";

   procedure P (X : New_Str) is
   begin
      Failed
        ("THE IMAGE ATTRIBUTE MUST RETURN A VALUE OF THE" &
         " PREDEFINED TYPE STRING");
   end P;

   procedure P (X : String) is
   begin
      null;
   end P;

begin
   Test ("C87B07E", "OVERLOADED OPERANDS TO THE IMAGE ATTRIBUTE");

   if Integer'Image (+12) &
     Integer'Image (-12) &
     New_Int'Image (+12) &
     New_Int'Image (-12) &
     Number'Image (+12) &
     Number'Image (-12) /=
     " 12-12-12-12 12 12"
   then
      Failed ("RESOLUTION INCORRECT FOR THE 'IMAGE' ATTRIBUTE");
   end if;

   P (Integer'Image (+1) & New_Int'Image (+1) & Number'Image (-1));

   Result;
end C87b07e;
