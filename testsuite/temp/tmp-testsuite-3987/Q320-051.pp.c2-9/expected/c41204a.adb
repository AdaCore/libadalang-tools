-- C41204A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF A SLICE'S DISCRETE
--   RANGE IS NOT NULL, AND ITS LOWER OR UPPER BOUND IS NOT A
--   POSSIBLE INDEX FOR THE NAMED ARRAY.

-- WKB 8/4/81
-- EDS 7/14/98    AVOID OPTIMIZATION

with Report; use Report;
procedure C41204a is

begin
   Test
     ("C41204A",
      "ILLEGAL UPPER OR LOWER BOUNDS FOR A " &
      "SLICE RAISES CONSTRAINT_ERROR");

   declare

      type T is array (Integer range <>) of Integer;
      A : T (10 .. 15) := (10, 11, 12, 13, 14, 15);
      B : T (-20 .. 30);

   begin

      begin
         B (Ident_Int (9) .. 12) := A (Ident_Int (9) .. 12);
         Failed ("CONSTRAINT_ERROR NOT RAISED - 1" & Integer'Image (B (10)));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION - 1");
      end;

      begin
         B (Ident_Int (-12) .. 14) := A (Ident_Int (-12) .. 14);
         Failed ("CONSTRAINT_ERROR NOT RAISED - 2" & Integer'Image (B (10)));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION - 2");
      end;

      begin
         B (11 .. Ident_Int (16)) := A (11 .. Ident_Int (16));
         Failed ("CONSTRAINT_ERROR NOT RAISED - 3" & Integer'Image (B (15)));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION - 3");
      end;

      begin
         B (17 .. 20) := A (Ident_Int (17) .. Ident_Int (20));
         Failed ("CONSTRAINT_ERROR NOT RAISED - 4" & Integer'Image (B (17)));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION - 4");
      end;
   end;

   Result;
end C41204a;
