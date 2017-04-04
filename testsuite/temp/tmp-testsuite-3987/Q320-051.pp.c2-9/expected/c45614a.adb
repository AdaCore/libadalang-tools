-- C45614A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE EXPONENT VALUE IN
-- AN INTEGER EXPONENTIATION IS NEGATIVE.
-- CHECK BOTH STATIC AND NONSTATIC EXPONENT VALUES.

-- AH  9/29/86
-- EDS 7/15/98    AVOID OPTIMIZATION

with Report; use Report;
procedure C45614a is
   Int : Integer := 1;
   Res : Integer := 0;
begin
   Test
     ("C45614A",
      "CONSTRAINT_ERROR IS RAISED FOR INTEGERS " &
      "HAVING A NEGATIVE EXPONENT");

   declare
      E1 : constant Integer := -5;
   begin
      Res := Int**E1;
      Failed ("CONSTRAINT_ERROR NOT RAISED - E1A " & Integer'Image (Res));

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("CONSTRAINT_ERROR NOT RAISED - E1B");
   end;

   declare
      E2 : Integer := 5;
   begin
      Res := Int**(-E2);
      Failed ("CONSTRAINT_ERROR NOT RAISED - E2A " & Integer'Image (Res));

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("CONSTRAINT_ERROR NOT RAISED - E2B");
   end;

   declare
      E3 : Integer;
   begin
      E3  := Ident_Int (-5);
      Res := Int**E3;
      Failed ("CONSTRAINT_ERROR NOT RAISED - E3A " & Integer'Image (Res));

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("CONSTRAINT_ERROR NOT RAISED - E3B");
   end;

   declare
   begin
      Res := Int**Ident_Int (-5);
      Failed ("CONSTRAINT_ERROR NOT RAISED - E4A " & Integer'Image (Res));

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("CONSTRAINT_ERROR NOT RAISED - E4B");
   end;

   Res := Ident_Int (2);
   Res := Ident_Int (Res);
   Result;
end C45614a;
