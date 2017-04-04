-- C43211A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF A BOUND IN A NON-NULL RANGE OF A
-- NON-NULL AGGREGATE DOES NOT BELONG TO THE INDEX SUBTYPE.

-- EG  02/06/84
-- EG  05/08/85
-- EDS 07/15/98 AVOID OPTIMIZATION

with Report;

procedure C43211a is

   use Report;

begin

   Test
     ("C43211A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED IF A " &
      "BOUND IN A NON-NULL RANGE OF A NON-NULL " &
      "AGGREGATE DOES NOT BELONG TO THE INDEX " &
      "SUBTYPE");

   declare

      subtype St is Integer range 4 .. 8;
      type Base is array (St range <>, St range <>) of Integer;
      subtype T is Base (5 .. 7, 5 .. 7);

      A : T;

   begin

      Case_A : begin

         A := (6 .. 8 => (4 .. 6 => 0));
         if A /= (6 .. 8 => (4 .. 6 => 0)) then
            Failed ("CASE A : INCORRECT VALUES");
         end if;

      exception

         when others =>
            Failed ("EXCEPTION RAISED: CASE A");

      end Case_A;

      Case_B : begin

         A := (6 .. Ident_Int (8) => (Ident_Int (4) .. 6 => 1));
         if A /= (6 .. Ident_Int (8) => (Ident_Int (4) .. 6 => 1)) then
            Failed ("CASE B : INCORRECT VALUES");
         end if;

      exception

         when others =>
            Failed ("EXCEPTION RAISED: CASE B");

      end Case_B;

      Case_C : begin

         A := (7 .. 9 => (5 .. 7 => Ident_Int (2)));
         Failed
           ("CONSTRAINT_ERROR NOT RAISED: CASE C " &
            Integer'Image (A (Ident_Int (7), 7)));

      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("EXCEPTION RAISED: CASE C");

      end Case_C;

      Case_D : begin

         A := (5 .. 7 => (3 .. 5 => Ident_Int (3)));
         Failed
           ("CONSTRAINT_ERROR NOT RAISED: CASE D " &
            Integer'Image (A (7, Ident_Int (5))));

      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("EXCEPTION RAISED: CASE D");

      end Case_D;

      Case_E : begin

         A := (7 .. Ident_Int (9) => (5 .. 7 => Ident_Int (4)));
         Failed
           ("CONSTRAINT_ERROR NOT RAISED: CASE E " &
            Integer'Image (A (Ident_Int (7), 7)));

      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("CASE E : EXCEPTION RAISED");

      end Case_E;

      Case_F : begin

         A := (5 .. 7 => (Ident_Int (3) .. 5 => Ident_Int (5)));
         Failed
           ("CONSTRAINT_ERROR NOT RAISED: CASE F " &
            Integer'Image (A (7, Ident_Int (5))));

      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("EXCEPTION RAISED: CASE F");

      end Case_F;

      Case_G : begin

         A :=
           (7 .. 8 => (5 .. 7 => Ident_Int (6)),
            9      => (5 .. 7 => Ident_Int (6)));
         Failed
           ("CONSTRAINT_ERROR NOT RAISED: CASE G " &
            Integer'Image (A (7, Ident_Int (7))));

      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("EXCEPTION RAISED: CASE G");

      end Case_G;

   end;

   Result;

end C43211a;
