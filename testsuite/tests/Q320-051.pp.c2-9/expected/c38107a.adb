-- C38107A.ADA

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
--     FOR AN INCOMPLETE TYPE WITH DISCRIMINANTS DECLARED IN THE
--     VISIBLE PART OF A PACKAGE OR IN A DECLARATIVE PART, CHECK THAT
--     CONSTRAINT_ERROR IS RAISED IF A DISCRIMINANT CONSTRAINT IS
--     SPECIFIED FOR THE TYPE AND ONE OF THE DISCRIMINANT VALUES DOES
--     NOT BELONG TO THE CORRESPONDING DISCRIMINANT'S SUBTYPE.

-- HISTORY:
--     BCB 01/21/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C38107a is

begin
   Test
     ("C38107A",
      "FOR AN INCOMPLETE TYPE WITH DISCRIMINANTS " &
      "DECLARED IN THE VISIBLE PART OF A PACKAGE OR " &
      "IN A DECLARATIVE PART, CHECK THAT CONSTRAINT_" &
      "ERROR IS RAISED IF A DISCRIMINANT CONSTRAINT " &
      "IS SPECIFIED FOR THE TYPE AND ONE OF THE " &
      "DISCRIMINANT VALUES DOES NOT BELONG TO THE " &
      "CORRESPONDING DISCRIMINANT'S SUBTYPE");

   begin
      declare
         package P is
            subtype Int6 is Integer range 1 .. 6;
            type T_Int6 (D6 : Int6);
            type Test is access T_Int6 (7);  -- CONSTRAINT_ERROR.
            type T_Int6 (D6 : Int6) is record
               null;
            end record;
         end P;
         use P;
      begin
         Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 1");
         declare
            T : P.Test := new T_Int6 (7);
         begin
            if Equal (T.D6, T.D6) then
               Comment ("DON'T OPTIMIZE T.D6");
            end if;
         end;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 1");
   end;

   begin
      declare
         subtype Int7 is Integer range 1 .. 7;
         type T_Int7 (D7 : Int7);
         type Test is access T_Int7 (8);       -- CONSTRAINT_ERROR.
         type T_Int7 (D7 : Int7) is record
            null;
         end record;
      begin
         Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 2");
         declare
            T : Test := new T_Int7 (6);
         begin
            if Equal (T.D7, T.D7) then
               Comment ("DON'T OPTIMIZE T.D7");
            end if;
         end;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 2");
   end;
   Result;
end C38107a;
