-- C64104B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER APPROPRIATE CIRCUMSTANCES
--    WITH RESPECT TO PARAMETERS OF RECORD TYPES.  SUBTESTS INVOLVE
--    ACTUAL RECORD PARAMETERS WHOSE CONSTRAINT VALUES ARE NOT EQUAL
--    TO THE CONSTRAINTS ON THEIR CORRESPONDING FORMAL PARAMETERS:
--        (A) IN PARAMETER, STATIC AGGREGATE.
--        (B) IN PARAMETER, DYNAMIC AGGREGATE.
--        (C) IN PARAMETER, VARIABLE.
--        (D) IN OUT PARAMETER, EXCEPTION RAISED ON CALL.
--        (E) OUT PARAMETER, EXCEPTION RAISED ON CALL.

-- DAS 2/11/81
-- SPS 10/26/82

with Report;
procedure C64104b is

   use Report;
   subtype Int is Integer range 0 .. 10;
   type Rec (N : Int := 0) is record
      A : String (1 .. N);
   end record;
   subtype Srec is Rec (N => 3);
   procedure P1 (R : in Srec) is
   begin
      Failed ("EXCEPTION NOT RAISED ON CALL TO P1");
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN PROCEDURE P1");
   end P1;

   procedure P2 (R : in out Srec) is
   begin
      Failed ("EXCEPTION NOT RAISED ON CALL TO P2");
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN PROCEDURE P2");
   end P2;

   procedure P3 (R : out Srec) is
   begin
      Failed ("EXCEPTION NOT RAISED ON CALL TO P3");
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN PROCEDURE P3");
   end P3;

begin

   Test
     ("C64104B",
      "CHECK RAISING OF CONSTRAINT_ERROR FOR " & "PARAMETERS OF RECORD TYPES");

   begin     -- (A)
      P1 ((2, "AA"));
      Failed ("EXCEPTION NOT RAISED IN SUBTEST (A)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN SUBTEST (A)");
   end; -- (A)

   begin     -- (B)
      P1 ((Ident_Int (2), "AA"));
      Failed ("EXCEPTION NOT RAISED IN SUBTEST (B)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN SUBTEST (B)");
   end; -- (B)

   declare   -- (C)
      R : Rec := (Ident_Int (2), "AA");
   begin     -- (C)
      P1 (R);
      Failed ("EXCEPTION NOT RAISED IN SUBTEST (C)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN SUBTEST (C)");
   end; -- (C)

   declare   -- (D)
      R : Rec := (Ident_Int (2), "AA");
   begin     -- (D)
      P2 (R);
      Failed ("EXCEPTION NOT RAISED IN SUBTEST (D)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN SUBTEST (D)");
   end; -- (D)

   declare   -- (E)
      R : Rec;
   begin     -- (E)
      P3 (R);
      Failed ("EXCEPTION NOT RAISED IN SUBTEST (E)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN SUBTEST (E)");
   end; -- (E)

   Result;

end C64104b;
