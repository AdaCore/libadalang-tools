-- C43214A.ADA

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
-- FOR A MULTIDIMENSIONAL AGGREGATE OF THE FORM (F..G => ""), CHECK
-- THAT CONSTRAINT_ERROR IS RAISED IF F..G IS NON-NULL AND
-- F OR G DO NOT BELONG TO THE INDEX SUBTYPE.

-- EG  02/10/1984
-- JBG 12/6/84
-- EDS 07/15/98     AVOID OPTIMIZATION

with Report;

procedure C43214a is

   use Report;

begin

   Test
     ("C43214A",
      "FOR A MULTIDIMENSIONAL AGGREGATE OF THE FORM " &
      "(F..G => """"), CHECK THAT CONSTRAINT ERROR " &
      "IS RAISED IF F..G IS NON-NULL AND NOT IN THE " &
      "INDEX SUBTYPE");

   declare

      subtype Sta is Integer range 4 .. 7;
      type Ta is
        array (Sta range 5 .. 6, Sta range 6 .. Ident_Int (4)) of Character;

      A : Ta := (5 .. 6 => "");

   begin

      Case_A : begin

         if (6 .. Ident_Int (8) => "") = A then
            Failed ("CASE A : CONSTRAINT_ERROR NOT RAISED");
         end if;
         Failed ("CASE A : CONSTRAINT_ERROR NOT RAISED - 2");

      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("CASE A : WRONG EXCEPTION RAISED");

      end Case_A;

      Case_B : begin

         A := (Ident_Int (3) .. 4 => "");
         Failed ("CASE B : CONSTRAINT_ERROR NOT RAISED");
         begin
            Failed
              ("ATTEMPT TO USE A " &
               Character'Val
                 (Ident_Int (Character'Pos (A (A'First (1), A'First (2))))));
         exception
            when others =>
               Failed ("CONSTRAINT_ERROR NOT RAISED AT PROPER PLACE");
         end;

      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("CASE B : WRONG EXCEPTION RAISED");

      end Case_B;

   end;

   Result;

end C43214a;
